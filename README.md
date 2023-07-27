# dmx512-tool
Naive tool demonstrating DMX512 access using a cheap device based on an FTDI clone

There's a couple of fundamental issues which I will touch on here.

I am using Linux with an FTDI clone chip to the RS485 required for electrical compatibility. I am trying to stick to the standard OS facilities, i.e. via serial.pp etc.

The DMX512 message comprises a start byte plus 512 data bytes. These go to one or more daisychained devices, with device-specific interpretation of the content.

If it were a genuine FTDI chip, it would be possible to write a serial number into it which could be used to identify a particular string of daisychained lights etc. As it is, if I had multiple devices it would be down to luck which one (i.e. /dev/ttyUSB0, /dev/ttyUSB1 etc.) was connected to which string of lights. As such, what I've got here should be considered very much a "toy implementation" or at best suitable for a very small application; however since I'm sticking to standard APIs it should be portable.

The "luvvies" who designed this specified a speed of 250,000 bits per second. Linux is supposed to be able to select non-standard speeds by a special ioctl, I can't get that working but the standard 230,400 rate is close enough for async comms (<1 bit timing error per character).

If a continual stream of messages is not being sent, say 20 per second, a light is likely to revert to a failsafe state. This means that any realistic control software either needs a daemon to drive the daisychain (with a foreground tool sending commands to it over a FIFO or socket) or needs a carefully-written interactive program.

Each message must be preceded by a break, if this is too long it will be interpreted as a bus failure and lights will go failsafe. It used to be that Linux had a fixed break of 250 mSec but this can now be set in mSec (-ve parameter to the SerBreak() procedure).

My program takes parameters on the command line and sends repeated messages to the light(s) for around five seconds. On completion, the lights can be expected to revert to a failsafe state. As such it's intended very much as a proof of concept.
