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
    when "web_event" then
      puts "web_event: #{body["msg"]}"
    when "job_event" then
      puts "job_event: #{body["msg"]}"
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
