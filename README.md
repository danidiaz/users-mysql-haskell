# users-mysql-haskell

This is a [mysql-haskell](http://hackage.haskell.org/package/mysql-haskell) backend for the [users](http://hackage.haskell.org/package/users) library. 

# Differences with other backends

This package uses a newtype instead of defining an orphan instance for
[UserStorageBackend](https://www.stackage.org/haddock/lts-7.9/users-0.5.0.0/Web-Users-Types.html#t:UserStorageBackend).

This means that the user will need to perform some manual wrapping and
unwrapping.

# Testing

The tests assume that there is an active mysql instance.

Rename the file tests/env.sh.template to tests/env.sh and fill in the required
values for the environment variables. Then run ./tests.sh.
