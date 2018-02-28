include $(abs_top_srcdir)/Makefrag

default: all
src_dir = .

tests = \
	echo \
	faulty-cycle

tests_p = $(addprefix $(ROCC)-p-, $(tests))

CFLAGS := $(CFLAGS) \
	-static \
	-mcmodel=medany \
	-fvisibility=hidden \
	-nostdlib \
	-nostartfiles \
	-I$(abs_top_srcdir) \
	-I$(RISCVTOOLS) \

all: $(tests_p)

vpath %.S $(src_dir)

$(ROCC)-p-%: %.S
	$(CC) $(CFLAGS) -I$(ENV_P) -T$(ENV_P)/link.ld -o $@ $<

$(ROCC)-v-%: %.S
	$(CC) $(CFLAGS) -I$(ENV_V) -T$(ENV_V)/link.ld -o $@ $<

junk += $(tests_p)

clean:
	rm -rf $(junk)
