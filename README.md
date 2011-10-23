Haskell interface to the Kinect.

I got my Kinect this weekend. While developing this I made a little
short-list of steps I took along the way to get libfreenect working
and examples and such:

    git clone https://github.com/OpenKinect/libfreenect.git
    sudo apt-get install libusb-1.0-0-dev
    sudo apt-get install freeglut3-dev
    sudo apt-get install libxmu-dev
    sudo apt-get install libxi-dev
    mkdir build
    cd build
    cmake ..
    make
    sudo add-apt-repository ppa:arne-alamut/freenect
    sudo adduser chris video

    sudo cp -R build/lib/* /usr/lib
    sudo apt-get install libcv-dev
    sudo apt-get install libhighgui-dev

Example program outputs:


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
