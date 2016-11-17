# cl-fuse

A daft hackathon project to allow live-coding of mobile apps using [Fuse](https://www.fusetools.com/) via lisp.

I work at Fusetools & we had a hackathon for a few days. I love playing with lisp and given that parenscript and various xml libraries exist I thought it would be fairly easy to cross compile from lisp to Fuse code.

## How to use

- [Download & Install Fuse](https://www.fusetools.com/downloads)

- Clone this project into your quicklisp `local-projects` folder

- `(ql:quickload :fuse)`

- `(fuse:create-app-project :my-app-name)` this will also quickload the result

- `(in-package :my-app-name)` we need to do this as the next command uses the current package to know what to load

- `(fuse:preview-app)`

- Open the `<my-app-name>.lisp` file in your project and start writing code :D
