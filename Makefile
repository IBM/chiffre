base_dir=$(abspath .)

tags_scala = \
	$(base_dir)/../src/main/scala \
	$(base_dir)/../chisel3 \
	$(base_dir)/src/main/scala

.PHONY: all clean default tags

default: all
all: $(base_dir)/utils/bin/scan-chain-config.jar

src_dir=$(base_dir)/src/main/scala

bin_deps= \
	$(src_dir)/leChiffre/scan/scan.scala \
	$(base_dir)/scan-chain-config/src/main/scala/ScanChainConfig.scala
$(base_dir)/utils/bin/scan-chain-config.jar: $(bin_deps)
	(cd $(base_dir)/scan-chain-config && sbt assembly)

tags:
	find $(tags_scala) -name *.scala -exec ctags --output-format=etags {} +
	find $(tags_scala) -name *.scala -exec ctags {} +

clean:
	rm -rf $(base_dir)/TAGS $(base_dir)/xtags
	rm -rf $(base_dir)/utils/bin/scan-chain-config.jar
