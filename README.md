# ErlCam 

ErlCam is a simple webcam capture library for Erlang. 

It wraps a simple C application I was calling  bitcam, which dumps binary to 
stdout based on the field/format/resolution you specify.

```
$ ./bitcam
BitCam, prints what your webcam sees. Just press Enter, or q to Quit.
Usage: ./bitcam <device> [ <height> <width> <format> <field> ]
Note that device should be the full path, i.e. /dev/video0

$ ./bitcam /dev/video0 720 480 none mjpeg > test.jpg
capture
q
$
```

The Erlang port wraps this and can request frames as Erlang binaries.

```erlang
> erlcam:start().
> {ok, Binary} = erlcam:get_frame( Port ).
> ok = file:write_file( "test.jpg", Binary ).
> erlcam:stop().
```

Additionally, you can set erlcam up as a streaming service via a pub/sub style
mechanims: 

```erlang
> GroupName = webcam.
> ok = pg2:create( GroupName ).
> ok = pg2:join( GroupName, self() ).
> erlcam:start().
> erlcam:add_group( GroupName, 1000 * 60 ). 
> FileName = fun() -> io_lib:format("~p.jpg",[erlang:unique_integer([positive])]) end.
> Save = fun(Bin) -> file:write_file(FileName(),Bin) end.
> F = fun This() -> receive {erlcam, frame, Bin} -> Save(Bin), This() end end.
> F().
```

That shows a server which pulls a snapshot of the camera once a minute.

## Credits ##

[OpenXC](https://github.com/openxc/android-webcam) - Android Webcam JNI 
Copyright (c) 2011-2013 Ford Motor Company Licensed under the BSD license.

## License ##

The BSD License above is the 'Revised' 3-Clause, which is GPL compatible. 

My portion (the Erlang) is all GPL'd, along with the additions and driver app.

