# Define the library
lib_LTLIBRARIES = libbutton90.la
libbutton90_la_SOURCES = pgbutt_viewport.f90 \
pgbutt_linked_viewports.f90 \
pgbutt_canvas.f90

# Define the program that depends on the library
bin_PROGRAMS = samplebutton
samplebutton_SOURCES = samplebutton.f90
samplebutton_LDADD = libbutton90.la

# Specify dependencies for the modules
BUILT_SOURCES = $(libbutton90_la_SOURCES:.f90=.mod)

# Static pattern rule to build .mod files from .f90 files
$(BUILT_SOURCES): %.mod: %.f90
	$(FC) -c $<

samplebutton.o: samplebutton.f90 $(BUILT_SOURCES)

# Specify the directory for module files
AM_CPPFLAGS = -I$(srcdir)

# Install .mod files
moddir = $(includedir)
mod_DATA = $(BUILT_SOURCES)

CLEANFILES = *.mod *.o
