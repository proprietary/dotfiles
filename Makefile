all: emacs-symlinks

.PHONY = emacs-symlinks
emacs-symlinks:
	@mkdir -p "$$HOME/.emacs.d"; \
	for f in "$(CURDIR)/emacs/"*; do \
		target="$$HOME/.emacs.d/$$(basename $$f)"; \
		if [ -L "$$target" ] && [ "$$(readlink "$f")" != "$$target" ]; then \
			rm "$$target"; \
		fi; \
		ln -sf "$${f}" "$$HOME/.emacs.d/$$(basename $$f)"; \
	done; \
