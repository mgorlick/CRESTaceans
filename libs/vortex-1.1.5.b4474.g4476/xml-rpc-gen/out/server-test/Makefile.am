INCLUDES = -g -I$(top_srcdir) $(LIBRARIES_CFLAGS)  -DCOMPILATION_DATE=`date +%s` -Wall

bin_PROGRAMS = server-test

EXTRA_DIST = conf.xml

server_test_LDADD = $(LIBRARIES_LIBS)

server_test_SOURCES = main.c service_dispatch.c service_dispatch.h test_sum_int_int.h test_sum_int_int.c test_operate_int_int.h test_operate_int_int.c 