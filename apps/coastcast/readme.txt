COASTcast is a sample application of Computational State Transfer (COAST).
COASTcast allows users to deploy streaming video as collections of video services, implemented with COAST mobile code.
How to use:

./startup.rkt [--host=HOSTNAME] [--port=PORTNUMBER] [--vhost=VHOSTNAME] [--vport=VPORTNUMBER] [--w=WIDTH] [--h=HEIGHT] [--no-gui] [--no-video]

where PORTNUMBER and HOSTNAME are IP ports and DNS names for the local COAST island to assume,
VPORTNUMBER and VHOSTNAME are IP ports and DNS addresses naming the COAST island where the camera reading/video encoding service should be sent,
WIDTH and HEIGHT specify a width and height to try to use in setting up the camera reading/video encoding service,
and --no-gui/--no-video tell the startup script to NOT start the local GUI service or the camera reading/video encoding service.



