# Haskell interface to the Kinect

The following instructions assume you are installing in a Linux environment (specifically, Ubuntu 10.04 32-bit). It also assumes that you already have Haskell/GHC installed.

## Dependencies

    sudo apt-get update
    sudo apt-get install cmake
    sudo apt-get install git-core
    sudo apt-get install libusb-1.0-0-dev
    sudo apt-get install freeglut3-dev
    sudo apt-get install libxmu-dev
    sudo apt-get install libxi-dev
    sudo apt-get install libcv-dev
    sudo apt-get install libhighgui-dev

## Installing libfreenect

To install the libfreenect library, first checkout the latest version of the source code from their github repository:

    git clone https://github.com/OpenKinect/libfreenect.git

Next, move into the source folder and build the library using cmake.

    cd libfreenect
    mkdir build
    cd build
    cmake ..

Install it:

    sudo make install

Lastly, ensure you never have to run libfreenect code as an administrator by copying the included udev rules:

    cd ..
    sudo cp platform/linux/udev/51-kinect.rules /etc/udev/rules.d

## Installing this library

First check out the code:

    git clone git@github.com:kevincon/freenect.git

Next, move into the folder and install using cabal:

    cd freenect
    cabal install freenect.cabal

## Installing the examples

In the freenect source folder:

    cd examples
    cabal install examples.cabal

If you encounter errors, you may need to install the glut and juicypixels hackage packages:

    cabal update
    cabal install glut
    cabal install juicypixels

You can also run these examples directly. First navigate to the src directory:

    cd src

To run the RGB.hs program, which grabs an RGB video frame from the Kinect and saves it as a file called "output.bmp" in the same directory, run:

    runhaskell RGB.hs

To run the GlutRGB.hs program, which displays the real-time RGB video stream from the Kinect in a window, run:

    runhaskell GlutRGB.hs
    
# TODO

* Couple parts in the Haskell FFI do some mallocation, must add the necessary free()-equivalent calls.
