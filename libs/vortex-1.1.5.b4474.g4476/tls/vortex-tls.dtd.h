/**
 * C inline representation for DTD tls.dtd, created by axl-knife
 */
#ifndef __TLS_DTD_H__
#define __TLS_DTD_H__
#define TLS_DTD "\n\
<!--                                                              \
   DTD for the TLS Transport Security Profile, as of 2000-09-04   \
                                                                  \
                                                                  \
   Refer to this DTD as:                                          \
                                                                  \
     <!ENTITY % TLS PUBLIC '-//IETF//DTD TLS//EN'                 \
                'http://xml.resource.org/profiles/TLS/tls.dtd'>   \
     %TLS;                                                        \
   -->                                                            \
                                                                  \
                                                                  \
<!--                                                              \
  TLS messages, exchanged as application/beep+xml                 \
                                                                  \
     role       MSG         RPY         ERR                       \
    ======      ===         ===         ===                       \
    I or L      ready       proceed     error                     \
   -->                                                            \
                                                                  \
                                                                  \
<!ELEMENT ready       EMPTY>                                      \
<!ATTLIST ready                                                   \
          version     CDATA              '1'>                     \
                                                                  \
<!ELEMENT proceed     EMPTY>                                      \
\n"
#endif
