HAD
===

HAD is GHC's devs companion tool. It is meant to be run in GHC's sources root
directory.

Install it with:

    > git clone https://github.com/hsyl20/had.git
    > cd had
    > stack install

Be sure that the path where stack has installed "had" (e.g., ~/.local/bin) is in
your PATH.

To use it:

    > cd ghc_home_directory
    > had -p 6789

Then navigate to "http://localhost:6789"
