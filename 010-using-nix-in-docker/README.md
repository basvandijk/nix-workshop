# Using Nix in Docker

Run the command

    docker build -t nix .

to build a container image with a proper Nix installation that can be used to
run all examples fromm this tutorial. Then use the command

    docker run -it --rm --name nix -h nix nix bash

to enter the virtual environment. We have tested this functiality with docker
version 17.06.1, but in all likeliehood earlier version will work fine, too.