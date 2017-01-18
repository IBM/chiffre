.PHONY: clean tags

base_dir=$(abspath .)

tags_scala = \
	$(base_dir)/../src/main/scala \
	$(base_dir)/../chisel3 \
	$(base_dir)/src/main/scala
tags_perl = \
	$(base_dir)/util/fault-injection/opt/lib/perl5/*/Verilog/

tags:
	find $(tags_perl) -name *.pm -exec ctags --output-format=etags {} +
	find $(tags_perl) -name *.pm -exec ctags {} +
	find $(tags_scala) -name *.scala -exec ctags --output-format=etags {} +
	find $(tags_scala) -name *.scala -exec ctags {} +

clean:
	rm -rf $(base_dir)/TAGS $(base_dir)/xtags
