# Erlang Examples

Erlang examples may be run in the UNIX shell or on supported microcontroller devices.

## `hello_world`

This example program prints the string "Hello World" and quits.

### Command line

The `hello_world.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/hello_world.avm
    Hello World
    Return value: ok

## `udp_server`

This example program listens on UDP port 44444 and will print information about the received message, including the source IP, (ephemeral) source port, and packet received, to the console.

### Command line

The `udp_server.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/udp_server.avm
    Opening socket ...
    {socket,<0.3.0>,44444}
    Waiting to receive data...

You can send UDP packets to the AtomVM instance using `netcat` (or `nc` on some platforms), in a separate terminal window:

    shell$ nc -u localhost 44444

This command will wait for you to enter a line of text, e.g.,

    testing 1 2 3

In the AtomVM termianl window, you see:

    Address: {127,0,0,1}
    Port: 50889
    Packet: <<116,101,115,116,105,110,103,32,49,32,50,32,51,10>>
    Waiting to receive data...

> Note.  Netcat appends a newline character at the end of the input, so the packet binary does not display as printable text.

## `udp_client`

This example program send the packet of data (":アトムＶＭ") over UDP to port 44444 on the loopback address every 5 seconds, in a loop.  The program will print a period (`.`) to the console, every time it sends a message.

This command may be used in tandem with the `udp_server` program to illustrate sending messages between AtomVM processes over UDP.

#### Command line

The `udp_client.avm` file will get created as part of a build.  This file may be supplied as an argument to the `AtomVM` command:

    shell$ ./src/AtomVM ./examples/erlang/udp_client.avm
    Opening socket...
    .................................

If you are running the `udp_server` program, you should see messages like the following printed to the console:

    Address: {127,0,0,1}
    Port: 52443
    Packet: <<58,-94,-56,-32,54,45>>
    Waiting to receive data...

> Note. AtomVM does not currently treat characters outside of the printable ASCII character set as printable characters.
