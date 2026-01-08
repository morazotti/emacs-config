(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("6a5584ee8de384f2d8b1a1c30ed5b8af1d00adcbdcd70ba1967898c265878acf"
	 "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
	 "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
	 "b54b64550215242ad351a5c7219f1b30d2cec3d1710a2121862ae6d51c7a1c12"
	 "a0724a2dbf003213ef1bd93c7df0f07a5368399554078fc85819b2f9f6152101"
	 "9cda0155ffd0cddac60729f5e821ba7955e1623ec7bcb12ca8e7219c33747e0f"
	 "aad354ecde2ba0f811443573c9e6b8a45cd4a1d413cfbe7e3766b7598359a68e"
	 "7bf34d114ec815e05a1ecb7f1acfd61ef453bfd27d12cc4c2babfa08ca1314da"
	 "c8863bf29722cf36c5d293ac723fe9698e16f778326d791516dd717951ea8cd6"
	 "bd17d0f6495c2626c493322ff37c64dc76ddb06c10dbadfead6a4f92775f00f3"
	 "d481904809c509641a1a1f1b1eb80b94c58c210145effc2631c1a7f2e4a2fdf4"
	 "e4a702e262c3e3501dfe25091621fe12cd63c7845221687e36a79e17cf3a67e0"
	 "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33"
	 "a40703f9d1adb7ee1500d3c33ac4d62144675505ae7fe98b18a5d9ff325ee369"
	 "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
	 "6e33d3dd48bc8ed38fd501e84067d3c74dfabbfc6d345a92e24f39473096da3f"
	 "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
	 "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a"
	 "dfb1c8b5bfa040b042b4ef660d0aab48ef2e89ee719a1f24a4629a0c5ed769e8"
	 "c8b3d9364302b16318e0f231981e94cbe4806cb5cde5732c3e5c3e05e1472434"
	 "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434"
	 "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176"
	 "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
	 "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
	 "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
	 default))
 '(display-line-numbers-type 'visual)
 '(notmuch-saved-searches
   '((:name "inbox" :query "tag:inbox" :key [105])
	 (:name "unread" :query "tag:unread" :key [117])
	 (:name "flagged" :query "tag:flagged" :key [102])
	 (:name "sent" :query "tag:sent" :key [116])
	 (:name "drafts" :query "tag:draft" :key [100])
	 (:name "all mail" :query "*" :key [97])
	 (:name "seleções arXiv Fanchini" :query
			"tag:selecao_arxiv_fanchini" :key [120])
	 (:name "Física Experimental A" :query "tag:91103-FisExpA-2025"
			:sort-order newest-first :search-type tree)
	 (:name "UFSCar" :query "tag:ufscar" :sort-order newest-first
			:search-type tree)))
 '(org-agenda-files nil)
 '(org-html-mathjax-template
   "<script>\12  window.MathJax = {\12    loader: {load: ['[tex]/physics']},\12    tex: {\12      ams: {\12        multlineWidth: '%MULTLINEWIDTH'\12      },\12      packages: {'[+]': ['physics']},\12      tags: '%TAGS',\12      tagSide: '%TAGSIDE',\12      tagIndent: '%TAGINDENT'\12    },\12    chtml: {\12      scale: %SCALE,\12      displayAlign: '%ALIGN',\12      displayIndent: '%INDENT'\12    },\12    svg: {\12      scale: %SCALE,\12      displayAlign: '%ALIGN',\12      displayIndent: '%INDENT'\12    },\12    extensions: [\"[Contrib]/physics/physics.js\"],\12    output: {\12      font: '%FONT',\12      displayOverflow: '%OVERFLOW'\12    }\12  };\12</script>\12\12<script\12  id=\"MathJax-script\"\12  async\12  src=\"%PATH\">\12</script>")
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(safe-local-variable-values
   '((eval setq org-image-actual-width 350)
	 (eval setq org-image-align 'center)
	 (eval setq org-image-actual-width 500)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(warning-minimum-level :error))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :extend nil :weight normal :height 1.0))))
 '(org-level-2 ((t (:inherit outline-1 :extend nil :weight normal :height 1.0))))
 '(org-level-3 ((t (:inherit outline-1 :extend nil :weight normal :height 1.0))))
 '(org-level-4 ((t (:inherit outline-1 :extend nil :weight normal :height 1.0))))
 '(org-level-5 ((t (:inherit outline-1 :extend nil :weight normal :height 1.0)))))
