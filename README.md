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

Other than that, it's done!

# EXAMPLE

Example Depth.hs program outputs (point of interest: Payload):

    $ freenect-haskell-example
    Devices: 1
    Selected devices: [Camera]
    Opened device 0.
    Setted depth callback.
    Creating EP 82 transfer #0
    Creating EP 82 transfer #1
    Creating EP 82 transfer #2
    Creating EP 82 transfer #3
    Creating EP 82 transfer #4
    Creating EP 82 transfer #5
    Creating EP 82 transfer #6
    Creating EP 82 transfer #7
    Creating EP 82 transfer #8
    Creating EP 82 transfer #9
    Creating EP 82 transfer #10
    Creating EP 82 transfer #11
    Creating EP 82 transfer #12
    Creating EP 82 transfer #13
    Creating EP 82 transfer #14
    Creating EP 82 transfer #15
    Write Reg 0x0105 <= 0x00
    Control cmd=0003 tag=0000 len=0004: 12
    Control reply: 12
    Write Reg 0x0006 <= 0x00
    Control cmd=0003 tag=0001 len=0004: 12
    Control reply: 12
    Write Reg 0x0012 <= 0x03
    Control cmd=0003 tag=0002 len=0004: 12
    Control reply: 12
    Write Reg 0x0013 <= 0x01
    Control cmd=0003 tag=0003 len=0004: 12
    Control reply: 12
    Write Reg 0x0014 <= 0x1e
    Control cmd=0003 tag=0004 len=0004: 12
    Control reply: 12
    Write Reg 0x0006 <= 0x02
    Control cmd=0003 tag=0005 len=0004: 12
    Control reply: 12
    Write Reg 0x0017 <= 0x00
    Control cmd=0003 tag=0006 len=0004: 12
    Control reply: 12
    Started depth stream.
    Processing…
    [Stream 70] Packet with flag: 71
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 72
    [Stream 70] Packet with flag: 75
    Got depth frame of size 422400/422400, 242/242 packets arrived, TS 6fe9ddcf
    Payload: fromList [832,832,833,833,834,834,834,835,836,838,838,839,839,840,838,838,838,839,839,839,839,840,84
    Finished processing events.
    Write Reg 0x0006 <= 0x00
    Control cmd=0003 tag=0007 len=0004: 12
    Control reply: 12
    EP 82 transfer complete, 15 left
    EP 82 transfer complete, 14 left
    EP 82 transfer complete, 13 left
    EP 82 transfer complete, 12 left
    EP 82 transfer complete, 11 left
    EP 82 transfer complete, 10 left
    EP 82 transfer complete, 9 left
    EP 82 transfer complete, 8 left
    EP 82 transfer complete, 7 left
    EP 82 transfer complete, 6 left
    EP 82 transfer complete, 5 left
    EP 82 transfer complete, 4 left
    EP 82 transfer complete, 3 left
    EP 82 transfer complete, 2 left
    EP 82 transfer complete, 1 left
    EP 82 transfer complete, 0 left
    $ 
