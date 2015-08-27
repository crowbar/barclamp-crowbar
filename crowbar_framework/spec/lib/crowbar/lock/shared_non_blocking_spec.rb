require "timeout"
require "rspec/expectations"
require "spec_helper"

RSpec::Matchers.define :be_unlocked do
  match do |actual|
    !actual.locked? && !File.exists?(actual.path)
  end
  failure_message do |actual|
    if actual.locked?
      "expected that #{actual} would not be locked"
    else
      "expected that #{actual.path} would not exist"
    end
  end
end

RSpec::Matchers.define :be_locked_by_me do
  match do |actual|
    actual.locked? &&
      File.exists?(actual.path) &&
      File.read(actual.path) =~ /acquired by #{actual.owner}$/
  end
  failure_message do |actual|
    if !actual.locked?
      return "expected that #{actual} would be locked"
    end
    if File.exists?(actual.path)
      return "expected that #{actual.path} would exist"
    end
    return "expected #{actual.path} to match " +
      "/acquired by #{actual.owner}$/:\n" +
      File.read(actual.path)
  end
end

RSpec::Matchers.define :be_locked_by_another do
  match do |actual|
    !actual.locked? &&
      File.exists?(actual.path) &&
      # We only call this matcher on locks which have already been
      # acquired and then released.
      File.read(actual.path) =~ /released by #{actual.owner}$/
  end
  failure_message do |actual|
    if actual.locked?
      return "expected that #{actual} would not be locked"
    end
    if !File.exists?(actual.path)
      return "expected that #{actual.path} would exist"
    end
    return "expected #{actual.path} to match " +
      "/released by #{actual.owner}$/:\n" +
      File.read(actual.path)
  end
end

describe Crowbar::Lock::SharedNonBlocking do
  def safe_pipe(cmd, stdin)
    out, err, status = Open3.capture3(cmd, stdin_data: stdin)
    unless status.success?
      raise Crowbar::Lock::SharedNonBlocking.exec_failure(
        'dummy', cmd, stdin, out, err, status
      )
    end
  end

  def local_executor
    racy_local_executor(0, 0)
  end

  def racy_local_executor(start_delay, end_delay)
    lambda do |node, cmd, shell|
      # Make the critical region inside the flock much longer.
      # Note that the command to be run has already been logged at this point,
      # so the modifications won't show up in log/test.log.
      shell = "sleep #{start_delay};\n" + shell if start_delay > 0
      shell += ";\nsleep #{end_delay}" if end_delay > 0
      safe_pipe(cmd, shell)
    end
  end

  let(:klass) { Crowbar::Lock::SharedNonBlocking }
  let(:defaults) do
    {
      executor: local_executor,
      node: "dummy", # local_executor ignores this
      owner: "rspec",
      reason: "testing",
    }
  end
  let(:lock) { new_lock }

  def new_lock(options = {})
    options[:path] ||= Dir::Tmpname.create(["test_shared_nb-", ".lock"]) {}
    # options[:logger] ||= Logger.new(STDOUT).tap do |logger|
    #   logger.level = Logger::DEBUG
    # end
    klass.new(defaults.merge(options))
  end

  after(:each) do
    lock.release
    lock.clean
  end

  context "when a lock is acquired" do
    it "returns a lock object" do
      expect(lock.acquire).to be_an_instance_of(klass)
    end

    it "sets a lock object to locked" do
      expect(lock).to be_unlocked
      lock.acquire
      expect(lock).to be_locked_by_me
    end

    it "works via a #with_lock block" do
      expect(lock).to be_unlocked
      lock.with_lock do
        expect(lock).to be_locked_by_me
      end
      expect(lock).to be_unlocked
    end

    it "is a shared lock" do
      lock2 = new_lock(path: lock.path, owner: lock.owner + "2")
      lock.acquire
      lock2.acquire
      expect(lock).to be_locked_by_me
      expect(lock2).to be_locked_by_me
      lock2.release
      expect(lock).to be_locked_by_me
      expect(lock2).to be_locked_by_another
    end

    it "can't be locked twice by one owner" do
      lock2 = new_lock(path: lock.path, owner: lock.owner)
      lock.acquire
      expect { lock2.acquire }.to raise_error(
        Crowbar::Error::LockingFailure,
        /#{lock.owner} already had lock on #{lock.node} /
      )
    end

    it "keeps locks with different paths independent" do
      # Check that one of two attempts racing to obtain lock will win
      lock2 = new_lock
      lock.acquire
      lock2.acquire
      expect(lock2).to be_locked_by_me
      lock2.release
      lock2.clean
    end

    it "protects against race conditions" do
      lock1 = new_lock(executor: racy_local_executor(2, 0))
      lock2 = new_lock(
        executor: racy_local_executor(0.5, 0),
        owner: lock1.owner + "2",
        path: lock1.path
      )

      thread = Thread.new { lock1.acquire }
      sleep 0.5 # make sure lock1 gets the flock first
      lock2.acquire # this should wait for lock1 to be acquired
      expect(lock1).to be_locked_by_me
      expect(lock2).to be_locked_by_me
      thread.join # this should happen instantly

      lock1.release
      lock2.release
      lock1.clean
    end
  end

  context "when a lockfile is released" do
    it "returns a lock object" do
      lock.acquire
      expect(lock.release).to be_an_instance_of(klass)
    end

    it "unlocks a lock object" do
      lock.acquire
      lock.release
      expect(lock).to be_unlocked
    end

    it "protects against race conditions" do
      lock1 = new_lock(executor: racy_local_executor(2, 0))
      lock2 = new_lock(
        executor: racy_local_executor(0.5, 0),
        owner: lock1.owner + "2",
        path: lock1.path
      )

      lock1.acquire
      lock2.acquire

      thread = Thread.new do
        lock1.release
        expect(lock1).to be_locked_by_another
      end
      sleep 0.5 # make sure lock1 gets the flock first
      lock2.release
      expect(lock1).to be_unlocked
      expect(lock2).to be_unlocked
      thread.join

      lock1.clean
    end
  end

  context "when there is a stale lock from the same owner" do
    before(:each) do
      Dir.mkdir lock.readers_dir unless File.directory? lock.readers_dir
      File.open(File.join(lock.readers_dir, lock.owner), "w") do |f|
        f.write "stale lock"
      end
    end

    it "should raise an exception" do
      expect { lock.acquire }.to \
        raise_error(/#{lock.owner} already had lock/)
    end
  end
end
