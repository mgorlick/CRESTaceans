set(SRC_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src)
set(SRC_REFMAN_DIR ${CMAKE_CURRENT_SOURCE_DIR}/src/refman)

# Put these in the order that they should appear in the Info or PDF manual.
set(PAGES
    getting_started

    config
    direct3d
    display
    events
    file
    fshook
    fixed
    graphics
    joystick
    keyboard
    memory
    misc
    mouse
    opengl
    path
    platform
    state
    system
    threads
    time
    timer
    transformations
    utf8

    audio
    acodec
    color
    font
    image
    native_dialog
    physfs
    primitives
    )

set(PAGES_TXT)
foreach(page ${PAGES})
    list(APPEND PAGES_TXT ${SRC_REFMAN_DIR}/${page}.txt)
endforeach(page)

set(IMAGES
    primitives1
    primitives2
    )


#-----------------------------------------------------------------------------#
#
#   Paths
#
#-----------------------------------------------------------------------------#

set(HTML_DIR ${CMAKE_CURRENT_BINARY_DIR}/html/refman)
set(MAN_DIR ${CMAKE_CURRENT_BINARY_DIR}/man)
set(INFO_DIR ${CMAKE_CURRENT_BINARY_DIR}/info)
set(TEXI_DIR ${CMAKE_CURRENT_BINARY_DIR}/texi)
set(LATEX_DIR ${CMAKE_CURRENT_BINARY_DIR}/latex)
set(PDF_DIR ${CMAKE_CURRENT_BINARY_DIR}/pdf)

set(PROTOS ${CMAKE_CURRENT_BINARY_DIR}/protos)
set(PROTOS_TIMESTAMP ${PROTOS}.timestamp)
set(HTML_REFS ${CMAKE_CURRENT_BINARY_DIR}/html_refs)
set(HTML_REFS_TIMESTAMP ${HTML_REFS}.timestamp)
set(DUMMY_REFS ${CMAKE_CURRENT_BINARY_DIR}/dummy_refs)
set(DUMMY_REFS_TIMESTAMP ${DUMMY_REFS}.timestamp)
set(SEARCH_INDEX_JS ${HTML_DIR}/search_index.js)

set(SCRIPT_DIR ${CMAKE_SOURCE_DIR}/docs/scripts)
set(MAKE_PROTOS ${CMAKE_CURRENT_BINARY_DIR}/make_protos)
set(MAKE_HTML_REFS ${CMAKE_CURRENT_BINARY_DIR}/make_html_refs)
set(MAKE_DUMMY_REFS ${CMAKE_CURRENT_BINARY_DIR}/make_dummy_refs)
set(MAKE_DOC ${CMAKE_CURRENT_BINARY_DIR}/make_doc --protos ${PROTOS})
set(INSERT_TIMESTAMP ${CMAKE_CURRENT_BINARY_DIR}/insert_timestamp)
set(MAKE_SEARCH_INDEX ${CMAKE_CURRENT_BINARY_DIR}/make_search_index)

set(DAWK_SOURCES scripts/aatree.c scripts/dawk.c scripts/trex.c)

add_executable(make_protos scripts/make_protos.c ${DAWK_SOURCES})
add_executable(make_html_refs scripts/make_html_refs.c ${DAWK_SOURCES})
add_executable(make_dummy_refs scripts/make_dummy_refs.c ${DAWK_SOURCES})
add_executable(make_doc
    scripts/make_doc.c
    scripts/make_man.c
    scripts/make_single.c
    ${DAWK_SOURCES})
add_executable(insert_timestamp
   scripts/insert_timestamp.c
   ${DAWK_SOURCES})
add_executable(make_search_index scripts/make_search_index.c ${DAWK_SOURCES})

#-----------------------------------------------------------------------------#
#
#   Protos
#
#-----------------------------------------------------------------------------#

# The protos file is a list of function prototypes and type declarations
# which can then be embedded into the documentation.

# Rebuilding the documentation whenever a source file changes is irritating,
# especially as public prototypes rarely change.  Thus we keep a second file
# called protos.timestamp which reflects the last time that the protos file
# changed.  We declare _that_ file as the dependency of other targets.

# We can get into a situation where the protos file is newer than the source
# files (up-to-date) but the protos.timestamp is older than the source files.
# If the protos and protos.timestamp files are identical then each time
# you run make, it will compare them and find them equal, so protos.timestamp
# won't be updated.  However that check is instantaneous.

file(GLOB_RECURSE ALL_SRCS
    ${CMAKE_SOURCE_DIR}/src/*.[ch]
    ${CMAKE_SOURCE_DIR}/src/*.[ch]pp
    ${CMAKE_SOURCE_DIR}/include/*.h
    ${CMAKE_SOURCE_DIR}/include/*.inl
    ${CMAKE_SOURCE_DIR}/addons/*.[ch]
    ${CMAKE_SOURCE_DIR}/addons/*.[ch]pp
    )

add_custom_command(
    OUTPUT ${PROTOS}
    DEPENDS ${ALL_SRCS} make_protos
    COMMAND ${MAKE_PROTOS} ${ALL_SRCS} > ${PROTOS}
    )

add_custom_command(
    OUTPUT ${PROTOS_TIMESTAMP}
    DEPENDS ${PROTOS}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${PROTOS} ${PROTOS_TIMESTAMP}
    )

# For testing.
add_custom_target(gen_protos DEPENDS ${PROTOS})

#-----------------------------------------------------------------------------#
#
#   HTML
#
#-----------------------------------------------------------------------------#

# The html_refs file contains link definitions for each API entry.
# It's used to resolve references across HTML pages.
# The search_index.js file contains definitions for the autosuggest widget.

if(PANDOC_STRIP_UNDERSCORES)
    set(STRIP_UNDERSCORES "--strip-underscores")
else()
    set(STRIP_UNDERSCORES "")
endif()

add_custom_command(
    OUTPUT ${HTML_REFS}
    DEPENDS ${PAGES_TXT} make_html_refs
    COMMAND ${MAKE_HTML_REFS} ${STRIP_UNDERSCORES} ${PAGES_TXT} > ${HTML_REFS}
    )

add_custom_command(
    OUTPUT ${HTML_REFS_TIMESTAMP}
    DEPENDS ${HTML_REFS}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${HTML_REFS} ${HTML_REFS_TIMESTAMP}
    )

add_custom_command(
    OUTPUT ${SEARCH_INDEX_JS}
    DEPENDS ${HTML_REFS_TIMESTAMP} make_search_index
    COMMAND ${MAKE_SEARCH_INDEX} ${HTML_REFS} > ${SEARCH_INDEX_JS}
    )

if(WANT_DOCS_HTML)
    foreach(inc inc.a inc.z)
        add_custom_command(
            OUTPUT ${inc}.html
            DEPENDS ${SRC_REFMAN_DIR}/${inc}.txt
            COMMAND ${PANDOC} ${SRC_REFMAN_DIR}/${inc}.txt -o ${inc}.html
            )
    endforeach(inc)

    set(HTML_PAGES)
    foreach(page ${PAGES} index)
        add_custom_command(
            OUTPUT ${HTML_DIR}/${page}.html
            DEPENDS
                ${PROTOS_TIMESTAMP}
                ${HTML_REFS_TIMESTAMP}
                ${SRC_REFMAN_DIR}/${page}.txt
                ${CMAKE_CURRENT_BINARY_DIR}/inc.a.html
                ${CMAKE_CURRENT_BINARY_DIR}/inc.z.html
                ${SEARCH_INDEX_JS}
                make_doc
                insert_timestamp
            COMMAND
                ${INSERT_TIMESTAMP} ${CMAKE_SOURCE_DIR}/include/allegro5/base.h > inc.timestamp.html
            COMMAND
                ${MAKE_DOC}
                --to html
                --raise-sections
                --include-before-body inc.a.html
                --include-after-body inc.timestamp.html
                --include-after-body inc.z.html
                --css pandoc.css
                --include-in-header ${SRC_DIR}/custom_header.html
                --standalone
                --toc
                -- ${SRC_REFMAN_DIR}/${page}.txt ${HTML_REFS}
                > ${HTML_DIR}/${page}.html
            )
        list(APPEND HTML_PAGES ${HTML_DIR}/${page}.html)
    endforeach(page)
    
    set(HTML_IMAGES)
    foreach(image ${IMAGES})
        add_custom_command(
            OUTPUT ${HTML_DIR}/images/${image}.png
            DEPENDS
                ${SRC_REFMAN_DIR}/images/${image}.png
            COMMAND 
                "${CMAKE_COMMAND}" -E copy
                "${SRC_REFMAN_DIR}/images/${image}.png" "${HTML_DIR}/images/${image}.png"
            ) 
         list(APPEND HTML_IMAGES ${HTML_DIR}/images/${image}.png)
    endforeach(image)
    
    add_custom_target(html ALL DEPENDS ${HTML_PAGES} ${HTML_IMAGES})

    foreach(file pandoc.css autosuggest.js)
        configure_file(
            ${SRC_DIR}/${file}
            ${HTML_DIR}/${file}
            COPY_ONLY)
    endforeach(file)
endif(WANT_DOCS_HTML)

#-----------------------------------------------------------------------------#
#
#   Man pages
#
#-----------------------------------------------------------------------------#

set(MANDIR "man" CACHE STRING "Install man pages into this directory")

if(WANT_DOCS_MAN)
    make_directory(${MAN_DIR})

    set(MAN_PAGES)
    foreach(page ${PAGES_TXT})
        # Figure out the man pages that would be generated from this file.
        file(STRINGS ${page} lines REGEX "# API: ")
        if(lines)
            string(REGEX REPLACE "[#]* API: " ";" entries ${lines})

            set(outputs)
            foreach(entry ${entries})
                list(APPEND outputs ${MAN_DIR}/${entry}.3)
            endforeach(entry)

            add_custom_command(
                OUTPUT ${outputs}
                DEPENDS ${PROTOS_TIMESTAMP} ${page} make_doc
                COMMAND ${MAKE_DOC} --to man -- ${page}
                WORKING_DIRECTORY ${MAN_DIR}
                )

            list(APPEND MAN_PAGES ${outputs})
        endif(lines)
    endforeach(page)

    add_custom_target(man ALL DEPENDS ${MAN_PAGES})

    install(FILES ${MAN_PAGES}
            DESTINATION ${MANDIR}/man3
            )
endif(WANT_DOCS_MAN)

#-----------------------------------------------------------------------------#
#
#   Info
#
#-----------------------------------------------------------------------------#

add_custom_command(
    OUTPUT ${DUMMY_REFS}
    DEPENDS ${PAGES_TXT} make_dummy_refs
    COMMAND ${MAKE_DUMMY_REFS} ${PAGES_TXT} > ${DUMMY_REFS}
    )

add_custom_command(
    OUTPUT ${DUMMY_REFS_TIMESTAMP}
    DEPENDS ${DUMMY_REFS}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${DUMMY_REFS} ${DUMMY_REFS_TIMESTAMP}
    )

add_custom_target(gen_dummy_refs DEPENDS ${DUMMY_REFS})

if(WANT_DOCS_INFO AND PANDOC_WITH_TEXINFO AND MAKEINFO)
    make_directory(${INFO_DIR})
    make_directory(${TEXI_DIR})

    add_custom_target(info ALL DEPENDS ${INFO_DIR}/refman.info)
    add_custom_command(
        OUTPUT ${INFO_DIR}/refman.info
        DEPENDS ${TEXI_DIR}/refman.texi
        COMMAND ${MAKEINFO}
                --paragraph-indent 0
                --no-split
                ${TEXI_DIR}/refman.texi
                -o ${INFO_DIR}/refman.info
        )
    add_custom_command(
        OUTPUT ${TEXI_DIR}/refman.texi
        DEPENDS ${PROTOS_TIMESTAMP} ${DUMMY_REFS_TIMESTAMP} ${PAGES_TXT}
                make_doc
        COMMAND ${MAKE_DOC}
                --to texinfo
                --standalone
                --
                ${DUMMY_REFS}
                ${PAGES_TXT}
                > ${TEXI_DIR}/refman.texi
        )
else()
    if(WANT_DOCS_INFO)
        message("Info documentation requires Pandoc 1.1+ and makeinfo")
    endif(WANT_DOCS_INFO)
endif(WANT_DOCS_INFO AND PANDOC_WITH_TEXINFO AND MAKEINFO)

#-----------------------------------------------------------------------------#
#
#   LaTeX (PDF)
#
#-----------------------------------------------------------------------------#

make_directory(${LATEX_DIR})

add_custom_target(latex DEPENDS ${LATEX_DIR}/refman.tex)
add_custom_command(
    OUTPUT ${LATEX_DIR}/refman.tex
    DEPENDS ${PROTOS_TIMESTAMP}
            ${DUMMY_REFS_TIMESTAMP}
            ${PAGES_TXT}
            ${SRC_REFMAN_DIR}/header.tex
            make_doc
    COMMAND ${MAKE_DOC}
            --to latex
            --include-in-header ${SRC_REFMAN_DIR}/header.tex
            --standalone
            --number-sections
            -- ${DUMMY_REFS} ${PAGES_TXT}
            > ${LATEX_DIR}/refman.tex
    )

if(WANT_DOCS_PDF AND PDFLATEX_COMPILER)
    make_directory(${PDF_DIR})

    add_custom_target(pdf ALL DEPENDS ${PDF_DIR}/refman.pdf)
    add_custom_command(
        OUTPUT ${PDF_DIR}/refman.pdf
        DEPENDS ${LATEX_DIR}/refman.tex
        # Repeat three times to get cross references correct.
        COMMAND ${PDFLATEX_COMPILER} -output-directory ${PDF_DIR} ${LATEX_DIR}/refman.tex
        COMMAND ${PDFLATEX_COMPILER} -output-directory ${PDF_DIR} ${LATEX_DIR}/refman.tex
        COMMAND ${PDFLATEX_COMPILER} -output-directory ${PDF_DIR} ${LATEX_DIR}/refman.tex
        )
else()
    if(WANT_DOCS_PDF)
        message("PDF generation requires pdflatex")
    endif(WANT_DOCS_PDF)
endif(WANT_DOCS_PDF AND PDFLATEX_COMPILER)

#-----------------------------------------------------------------------------#
#
#   Tags file
#
#-----------------------------------------------------------------------------#

if(CTAGS)
    add_custom_target(gen_tags DEPENDS tags)
    add_custom_command(
        OUTPUT tags
        DEPENDS ${PAGES_TXT}
        COMMAND ${CTAGS}
            --langdef=allegrodoc
            --langmap=allegrodoc:.txt
            "--regex-allegrodoc=/^#+ API: (.+)/\\1/"
            ${PAGES_TXT}
        VERBATIM
        )
endif(CTAGS)

#-----------------------------------------------------------------------------#
#
#   Consistency check
#
#-----------------------------------------------------------------------------#

add_custom_target(check_consistency
    DEPENDS ${PROTOS}
    COMMAND ${SH} ${SCRIPT_DIR}/check_consistency --protos ${PROTOS}
            ${PAGES_TXT}
    )

#-----------------------------------------------------------------------------#
# vim: set sts=4 sw=4 et:
