
# Start rails
cd ..
script/rails s Puma &
sleep 10
cd BDD

# Run Tests
erl -s bdd test "crowbar" -s init stop -noshell

# STop rails
kill `ps -ef | grep Puma | grep ruby | awk '{ print $2 }'`
