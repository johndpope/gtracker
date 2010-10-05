ITEMS = static templates gtracker .htaccess index2.php page.php lang client utils avatars css js test.php
GROUP = www-data
OWNER = www-data
OUTPUT = root.tar.bz2
PERM = a+rw
WWW_ROOT = /var/www

root: $(OUTPUT)

$(OUTPUT): $(ITEMS)
	tar cvjpf $(OUTPUT) --exclude=".svn" --group $(GROUP) --owner $(OWNER) $(ITEMS)

install: $(OUTPUT)
	rm -rf $(WWW_ROOT)/*
	tar xvjpf $(OUTPUT) -C $(WWW_ROOT)/
	mkdir -p $(WWW_ROOT)/cache $(WWW_ROOT)/temp $(WWW_ROOT)/manual
	cp ../doc/gTracker\ protocol\ \(ver.\ 1\).pdf $(WWW_ROOT)/manual/api.pdf
	chown -R $(OWNER):$(GROUP) $(WWW_ROOT)/temp $(WWW_ROOT)/cache $(WWW_ROOT)/manual
	chmod 770 $(WWW_ROOT)/temp $(WWW_ROOT)/cache

clean:
	rm -rf $(OUTPUT)

deploy: clean install
