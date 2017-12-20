include $(abs_top_srcdir)/Makefrag

default: all
src_dir ?= .

_tests = $(notdir $(wildcard $(abs_top_srcdir)/../../xfiles-dana/build/nets/*-fixed.ant.h))
tests = \
	$(patsubst %-fixed.ant.h,%, $(_tests))

tests_p = $(addprefix $(ROCC)-p-, $(tests))

CFLAGS := $(CFLAGS) \
	-static \
	-mcmodel=medany \
	-fvisibility=hidden \
	-nostdlib \
	-nostartfiles \
	-I$(abs_top_srcdir)/.. \
	-I$(abs_top_srcdir)/../../xfiles-dana

chiffre_bitstream ?= $(src_dir)/chiffre-firrtl.h

LIBS := $(LIBS) \
	-lxfiles-user \
	-lxfiles-supervisor \

all: $(tests_p)

vpath %.S $(src_dir)
vpath %.ant.h $(abs_top_srcdir)/../../xfiles-dana/build/nets

$(ROCC)-p-%: faulty-transaction.S %-fixed.ant.h $(abs_top_srcdir)/../../xfiles-dana/tests/libs/build/$(TARGET)/libxfiles-user.a $(abs_top_srcdir)/../../xfiles-dana/tests/libs/build/$(TARGET)/libxfiles-supervisor.a
	$(CC) $(CFLAGS) -I$(ENV_P) -include $(chiffre_bitstream) -include $(abs_top_srcdir)/../../xfiles-dana/build/nets/$*-fixed.ant.h $< $(LDFLAGS) -L$(abs_top_srcdir)/../../xfiles-dana/tests/libs/build/$(TARGET) -T$(ENV_P)/link.ld -o $@ $(LIBS)

junk += $(tests_p)

clean:
	rm -rf $(junk)
