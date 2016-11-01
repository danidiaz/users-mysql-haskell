# users-mysql-haskell

This library is a mysql-haskell backend for the "users" library. 

# Differences with other "users" backends

This package uses a newtype instead of defining an orphan instance for
UserStorageBackend.

This means that the user will need to perform some manual wrapping and
unwrapping.

# Testing

The tests assume that there is an active mysql instance.

Rename the file tests/env.sh.template to tests/env.sh and fill in the required
values for the environment variables. Then run ./tests.sh.
