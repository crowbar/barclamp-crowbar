# encoding: utf-8

require "rubygems"
gem "amqp"
require "amqp"
require "json"

t = Thread.new { EventMachine.run }
sleep(0.5)


connection = AMQP.connect
channel = AMQP::Channel.new(connection, :auto_recovery => true)
channel.on_error do |ch, channel_close|
  raise "Channel-level exception: #{channel_close.reply_text}"
end

channel.prefetch(1)

channel.queue("", :durable => false, :auto_delete => true).bind("amqpgem.patterns.events").subscribe do |metadata, payload|
  begin
    body = JSON.parse(payload)

    case metadata.type
    when "warmup" then
      puts "WARMUP: #{body["msg"]}"
    when "page_request" then
      puts "page_request: #{body["msg"]}"
    when "widgets.created" then
      puts "A widget #{body[:id]} was created"
    when "widgets.destroyed" then
      puts "A widget #{body[:id]} was destroyed"
    when "files.created" then
      puts "A new file (#{body[:filename]}, #{body[:sha1]}) was uploaded"
    when "files.indexed" then
      puts "A new file (#{body[:filename]}, #{body[:sha1]}) was indexed"
    else
      puts "[warn] Do not know how to handle event of type #{metadata.type}"
    end
  rescue Exception => e
    puts "[error] Could not handle event of type #{metadata.type}: #{e.inspect}"
  end
end

puts "[boot] Ready"
Signal.trap("INT") { connection.close { EventMachine.stop } }
t.join
