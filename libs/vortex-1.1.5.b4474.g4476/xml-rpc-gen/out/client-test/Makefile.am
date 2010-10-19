INCLUDES = -g -I$(top_srcdir) $(LIBRARIES_CFLAGS)  -DCOMPILATION_DATE=`date +%s` -Wall

libtest_xml_rpcincludedir = $(includedir)/test_xml_rpc

lib_LTLIBRARIES = libtest_xml_rpc.la

libtest_xml_rpc_la_SOURCES = test_xml_rpc.h test_xml_rpc.c test_types.h 

libtest_xml_rpc_la_LIBADD = $(LIBRARIES_LIBS)

EXTRA_DIST = test_xml_rpc.pc.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA =  test_xml_rpc.pc
