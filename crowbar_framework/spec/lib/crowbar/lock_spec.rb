require "spec_helper"
require "timeout"

describe Crowbar::Lock do
  let(:lock) { subject.class.new }

  after(:each) do
    lock.release
  end

  context "when a lock is acquired" do
    it "returns a lock object" do
      expect(lock.acquire).to be_an_instance_of(subject.class)
    end

    it "sets a lock object to locked" do
      expect(lock.locked?).to be false
      lock.acquire
      expect(lock.locked?).to be true
    end

    it "works via a #with_lock block" do
      expect(lock.locked?).to be false
      lock.with_lock do
        expect(lock.locked?).to be true
      end
      expect(lock.locked?).to be false
    end

    it "is an exclusive lock" do
      # Check that one of two attempts racing to obtain lock will win
      lock2 = subject.class.new
      lock.acquire
      expect {
        Timeout::timeout(1) do
          lock2.acquire
        end
      }.to raise_error(Timeout::Error)
      expect(lock2.locked?).to be false
      lock2.release
    end

    it "keeps locks with different paths independent" do
      # Check that one of two attempts racing to obtain lock will win
      lock2 = subject.class.new(path: lock.path.to_s + "2")
      lock.acquire
      lock2.acquire
      expect(lock2.locked?).to be true
      lock2.release
    end
  end

  context "when a lockfile is released" do
    it "returns a lock object" do
      lock.acquire
      expect(lock.release).to be_an_instance_of(subject.class)
    end

    it "unlocks a lock object" do
      lock.acquire
      lock.release
      expect(lock.locked?).to be false
    end

    it "can be acquired again" do
      lock.acquire
      lock.release
      lock.acquire
      expect(lock.locked?).to be true
    end
  end
end
