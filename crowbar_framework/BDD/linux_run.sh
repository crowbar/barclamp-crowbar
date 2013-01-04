
# Start rails
cd ..
bundle exec script/rails s Puma >> log/test.out 2>> log/test.err &
sleep 10
cd BDD

# Run Tests
erl -s bdd test crowbar -s init stop -noshell

# STop rails
kill `ps -ef | grep Puma | grep ruby | awk '{ print $2 }'`
