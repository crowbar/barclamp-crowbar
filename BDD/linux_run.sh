
# Start rails
cd ../crowbar_framework
bundle exec script/rails s Puma >> log/test.out 2>> log/test.err &
sleep 10
cd ../BDD

# Run Tests
erl -s bdd test devtool -s init stop -noshell

# Stop rails
kill `ps -ef | grep Puma | grep ruby | awk '{ print $2 }'`
