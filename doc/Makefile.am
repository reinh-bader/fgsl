SUBDIRS = examples


fgsl_docdir = $(datadir)/doc/fgsl
fgsl_doc2dir = $(datadir)/doc/fgsl/html
dist_fgsl_doc_DATA = refman.pdf 


install-data-local:
	$(MKDIR_P) $(DESTDIR)$(fgsl_doc2dir)
	$(MKDIR_P) $(DESTDIR)$(fgsl_doc2dir)/search
	$(INSTALL_DATA) $(srcdir)/html/*.css $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/*.html $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/*.js $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/*.map $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/*.md5 $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/*.png $(DESTDIR)$(fgsl_doc2dir)
	$(INSTALL_DATA) $(srcdir)/html/search/*.css $(DESTDIR)$(fgsl_doc2dir)/search
	$(INSTALL_DATA) $(srcdir)/html/search/*.html $(DESTDIR)$(fgsl_doc2dir)/search
	$(INSTALL_DATA) $(srcdir)/html/search/*.js $(DESTDIR)$(fgsl_doc2dir)/search
	$(INSTALL_DATA) $(srcdir)/html/search/*.png $(DESTDIR)$(fgsl_doc2dir)/search

uninstall-local:
	rm -rf $(DESTDIR)$(fgsl_doc2dir)

EXTRA_DIST = html
