/*
a dump of the capabilities of the “vorbisdec” element
You will see two pads: a source and a sink pad. Both of these pads are
always available, and both have capabilities attached to them. The sink pad will accept vorbis-encoded
audio data, with the mime-type “audio/x-vorbis”. The source pad will be used to send raw (decoded)
audio samples to the next element, with a raw audio mime-type (in this case, “audio/x-raw-int”).
*/

Pad Templates:
SRC template: ’src’
Availability: Always
Capabilities:
audio/x-raw-float
rate:
channels:
endianness:
width:
buffer-frames:
[ 8000, 50000 ]
[ 1, 2 ]
1234
32
0
SINK template: ’sink’
Availability: Always
Capabilities:
audio/x-vorbis

