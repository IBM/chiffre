base_dir=$(abspath .)

tags_scala = \
	$(base_dir)/../src/main/scala \
	$(base_dir)/../chisel3 \
	$(base_dir)/src/main/scala

fat_jar = $(base_dir)/utils/bin/chiffre.jar

.PHONY: all checkstyle clean default tags

default: all
all: $(fat_jar)

src_dir=$(base_dir)/src/main/scala

$(base_dir)/utils/bin/chiffre.jar:
	sbt assembly

tags:
	find $(tags_scala) -name *.scala -exec ctags --output-format=etags {} +
	find $(tags_scala) -name *.scala -exec ctags {} +

clean:
	rm -rf $(base_dir)/TAGS $(base_dir)/tags $(fat_jar)

checkstyle:
	sbt scalastyle test:scalastyle
