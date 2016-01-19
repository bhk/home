#!/bin/bash
:; for v in "${@//!/!1}" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; SCAM_ARGS=${a[*]} exec make --no-print-directory -f"$0"


define ///core.min
~eq = $(if $(findstring 1$1,$(findstring 1$2,1$1)),1)
~identity = $1
~xor = $(if $1,$(if $2,,$1),$2)
~concat-vec = $(call ~promote,$(subst $  ,$(call ~demote,$2),$1))
~cons = $(call ~demote,$1)$(if $2, )$2
~conj = $1$(if $1, )$(call ~demote,$2)
~last = $(call ~promote,$(lastword $1))
~butlast = $(wordlist 2,$(words $1),X $1)
~map-call = $(foreach x&,$2,$(call ^d,$(call $1,$(call ^u,$(x&)))))
~select-vec = $(filter-out !,$(foreach dx,$2,$(if $(call ^Y,$(call ~promote,$(dx)),,,,,,,,,$1),$(dx),!)))
~select-words = $(foreach a,$(foreach x,$2,$(if $(call ^Y,$x,,,,,,,,,$1),$x)),$a)
~vec-or = $(call ^u,$(word 1,$(filter-out !.,$1)))
~indicesX = $(if $(word $(words $2),$1),$(words $2) $(call ~indicesX,$1,. $2))
~indices = $(call ~indicesX,$1,.,1)
~rev-by-10s = $(if $1,$(if $2,$(foreach p,10 9 8 7 6 5 4 3 2 1,$(call ~rev-by-10s,$(wordlist $(word $p,0 1 2 3 4 5 6 7 8 9)$(patsubst %0,%1,$2),$p$2,$1),$(patsubst 0%,%,$2))),$(foreach p,10 9 8 7 6 5 4 3 2 1,$(word $p,$1))))
~rev-zeroes = $(if $(word 1$21,$1),$(call ~rev-zeroes,$1,0$2),$2)
~reverse = $(wordlist 1,99999999,$(call ~rev-by-10s,$1,$(call ~rev-zeroes,$1,)))
~while-0 = $(if $(filter iiiiiiiiiiiiiiiiiiii,$4),1 $(call ^d,$3),$(if $(call ^Y,$3,,,,,,,,,$1),$(call ~while-0,$1,$2,$(call ^Y,$3,,,,,,,,,$2),i$4),0 $(call ^d,$3)))
~while-N = $(if $(filter 0,$(word 1,$3)),$3,$(if $(filter iii,$5),$(if $(filter 1,$4),$(call ~while-N,$1,$2,$3,$4 0,ii),$3),$(call ~while-N,$1,$2,$(if $4,$(call ~while-N,$1,$2,$3,$(wordlist 2,99999999,$4)),$(call ~while-0,$1,$2,$(call ~nth,2,$3))),$4,i$5)))
~while = $(if $(call ^Y,$3,,,,,,,,,$1),$(call ^Y,$(call ^Y,$3,,,,,,,,,$2),,,,,,,,,$$(call ~nth,2,$$(call ~while-N,$(call ^e,$1),$(call ^e,$2),$$(call ~while-0,$(call ^e,$1),$(call ^e,$2),$$1),1,ii))),$3)
~isnumber = $(if $(filter 0% 1% 2% 3% 4% 5% 6% 7% 8% 9%,$(subst -,,$1)),$(if $(patsubst .%,%,$(patsubst %e,%,$(subst 0,,$(patsubst -%,%,$(subst $  ,_,$(subst E0,e,$(subst E-,E,$(subst e,E,$(subst 9,0,$(subst 8,0,$(subst 7,0,$(subst 6,0,$(subst 5,0,$(subst 4,0,$(subst 3,0,$(subst 2,0,$(subst 1,0,$1))))))))))))))))),,$1))
~append = $(filter %,$1 $2 $3 $4 $5 $6 $7 $8 $(if $(value 9),$(call ~promote,$(value 9))))
~hash-bind = $(subst %,!8,$(call ^d,$1))!=$(call ^d,$2)$(if $3, )$3
~hash-key = $(call ~promote,$(subst !8,%,$(word 1,$(subst !=, ,$1))))
~hash-value = $(call ~nth,2,$(subst !=, ,$1))
~hash-find = $(word 1,$(filter $(subst %,!8,$(call ^d,$1))!=%,$2))
~hash-get = $(call ~nth,2,$(subst !=, ,$(call ~hash-find,$1,$2))$(if $3, x $(call ~demote,$3)))
~hash-compact = $(if $(if $1,,1),$2,$(call ~append,$(word 1,$1),$(call ~hash-compact,$(filter-out $(word 1,$(subst !=,!=% ,$(word 1,$1))),$(wordlist 2,99999999,$1)))))
define ~format
$(or $(if $(findstring !,$1),$(if $(call ~eq,$1,$(foreach w,$1,$(call ~demote,$(call ~promote,$w)))),[$(foreach w,$1,$(call ~format,$(call ~promote,$w)))],$(if $(findstring !=,$(word 1,$1)),{$(call ~concat-vec,$(foreach e,$1,$(call ^d,$(call ~format,$(call ~promote,$(subst !8,%,$(word 1,$(subst !=, ,$e))))): $(call ~format,$(call ~nth,2,$(subst !=, ,$e))))),$(if ,,, ))}))),$(call ~isnumber,$1),"$(subst $ 	,\t,$(subst 
,\n,$(subst ",\",$(subst \,\\,$1))))")
endef
~vsprintf = $(call ~concat-vec,$(foreach w,$(join !. $(wordlist 2,99999999,$1),$(subst $  !% !%,%,$(subst %, !%,%s$(word 1,$1)))),$(if $(findstring !%s,$w),$(subst !%s,,$w),$(if $(findstring !%q,$w),$(call ~demote,$(call ~format,$(call ^u,$(word 1,$(subst !%q, ,$w)))))$(word 2,$(subst !%q,!. ,$w)),$(if $(findstring !%,$w),$(and $(info ** Warning: bad format string: '$(call ~nth,1,$1)')1,$(word 2,$(subst !%,! %,$w))))))))
~sprintf = $(call ~vsprintf,$(^av))
~printf = $(info $(call ~vsprintf,$(^av)))
define ~expect-x
$(if $(call ~eq,$1,$2),$(if $(findstring O,$(SCAM_DEBUG)),$(info OK: $(call ~format,$1))),$(and $(info $3: error: assertion failed
A: $(call ~format,$1)
B: $(call ~format,$2)
)1,$(error )))
endef
define ~see
$(if $(findstring $1,$2),1,$(and $(info Expected: $(subst 
,
          ,$1))1,$(info $   Within: $(subst 
,
          ,$2))))
endef
~uniq-x = $(if $1,$(word 1,$1) $(call ~uniq-x,$(filter-out $(word 1,$1),$(wordlist 2,99999999,$1))))
~uniq = $(subst ~1,~,$(subst ~p,%,$(filter %,$(call ~uniq-x,$(subst %,~p,$(subst ~,~1,$1))))))
~split = $(foreach w,$(subst $(call ^d,$1),!. !.,$(call ^d,$2)),$(or $(subst !.,,$w),!.))
~1+ = $(if $(filter %1 %2 %3 %4,$1),$(subst 1~,2,$(subst 2~,3,$(subst 3~,4,$(subst 4~,5,$1~)))),$(if $(filter %5 %6 %7,$1),$(subst 5~,6,$(subst 6~,7,$(subst 7~,8,$1~))),$(if $(findstring 9~,$1~),$(call ~1+,$(or $(subst 9~,,$1~),0))0,$(patsubst %0,%1,$(patsubst %8,%9,$1)))))
~mcache = $(and $(if $6,$(info Warning: memoized function passed more than three arguments))1,$(if $(if $(if $(filter-out u%,$(flavor $1)),1),,1),$(call ~set-global,$1,$(call ^Y,$3,$4,$5,,,,,,,$2)))1,$(value $1))
~memoenc = $(if $(or $1,$2,$3),~~$(subst ~,~0,$1)$(call ~memoenc,$2,$3))
~memoize = $(if $(if $(if $(filter-out u%,$(flavor $1)),1),,1),$(info Warning: [memoize-1] function '$1' not defined.),$(call ^Y,$(value $1),*memo$(call ~memoenc,$1),$1,,,,,,,$$(call ~set-rglobal,$$3,$$$$(call ~mcache,$$(call ^e,$$2)$$$$(call ~memoenc,$$$$1,$$$$2,$$$$3),$$(call ^e,$$1),$$$$1,$$$$2,$$$$3,$$$$(or $$$$4,$$$$5,$$$$6,$$$$7,$$$$8)))))
~sort-by = $(filter-out %!!,$(subst !!,!! ,$(sort $(foreach w,$2,$(call ~demote,$(call ^Y,$(call ~promote,$w),,,,,,,,,$1))!!$w))))
~assoc-initial = $(call ~promote,$(firstword $(if $(findstring %,$1),$(subst !8,$1,$(filter !8 !8!0%,$(subst $1,!8,$2))),$(filter $1 $1!0%,$2))))

endef
 
define ///make.min
$(call ^require,core)
top := top
linked-dirs := .emacs.d .config
vec-relpath = $(if $(and $1,$(call ~eq,$(call ^u,$(word 1,$1)),$(call ^u,$(word 1,$2)))),$(call vec-relpath,$(wordlist 2,99999999,$1),$(wordlist 2,99999999,$2)),$(call ~append,$(foreach x&,$1,$(call ^d,..)),$2))
relpath = $(call ~concat-vec,$(call vec-relpath,$(call ~split,/,$(abspath $1)),$(call ~split,/,$(abspath $2))),/)
find-files = $(foreach name,$1,$(call ^Y,$(wildcard $(name)/.* $(name)/*),,,,,,,,,$$(if $$1,$$(call find-files,$$(filter-out %/.. %/.,$$1)),$$(name))))
is-symlink? = $(shell if [ -L '$1' ] ; then echo 1 ; fi)
help-str := Usage:$!   make help         Display this message$!   make install      Install symbolic links$!   make uninstall    Remove symbolic links
install-file = $(and $(if $(if $(wildcard $(dir $1)),,1),$(and $(info Creating directory: $(dir $1))1,$(shell mkdir -p $(dir $1))))1,$(if $(and $(wildcard $1),$(if $(call is-symlink?,$1),,1)),$(info AVOIDING $2 (remove pre-existing file first)),$(and $(shell ln -fs $(call relpath,$(dir $1),$(top))/$2 $1)1,$(info Installed $2))))
uninstall-file = $(if $(call is-symlink?,$1),$(and $(info Removing $1)1,$(shell rm $1)),$(if $(wildcard $1),$(info LEAVING $1 (not a symlink)),$(info Missing $1)))
visit-files = $(foreach f,$(call ~append,$(filter-out $(addsuffix /%,$(linked-dirs)),$(patsubst $(top)/%,%,$(call find-files,$(top)))),$(linked-dirs)),$(call ^Y,$(HOME)/$f,$f,,,,,,,,$1))
rules := help install uninstall
build = $(if $(call ~eq,$1,help),$(info $(help-str)),$(if $(call ~eq,$1,install),$(call visit-files,$(value install-file)),$(if $(call ~eq,$1,uninstall),$(call visit-files,$(value uninstall-file)))))
define main
$(and $(foreach r,$(rules),$(eval $(subst X,$r,.PHONY: X
X: ; @true $$(call build,X)
)))1,$(eval Makefile: make.scm; @top/local/bin/scam -o $$@ $$< && rm *.min))
endef

endef
 
define ///runtime.min
SCAM_DEBUG ?=
$(if $(if $(findstring R,$(SCAM_DEBUG)),$(info runtime: $(lastword $(MAKEFILE_LIST)))),)
define \n


endef
 [ := (
 ] := )
& := \#
! := $(\n)

^d = $(or $(subst $  ,!0,$(subst $ 	,!+,$(subst !,!1,$1))),!.)
^u = $(subst !1,!,$(subst !+,	,$(subst !0, ,$(subst !.,,$1))))
^n = $(call ^u,$(word $1,$2))
^Y = $(call if,,,$(10))
^av = $(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,$(foreach n,1 2 3 4 5 6 7 8,$(call ^d,$($n)))$(if $9, $9) !)))
~^apply = $(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,9999,$2),$1)
~^f = "$(subst ",\",$(subst \,\\,$1))"
^tp = $(info $1 $(call ~^f,$2))$2
~^tc = $(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))
^ta = $(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(~^f)$(call ~^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9))
^t = $(info --> ($1$(call ~^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^tp,<-- $1:,$(call ~^tc,$1,$2,$3,$4,$5,$6,$7,$8,$9))
define ~esc-LHS
$$(if ,,$(subst $],$$],$(subst $[,$$[,$(subst 
,$$!,$(subst #,$$&,$(subst $$,$$$$,$1))))))
endef
define ^set
$(eval $(call ~esc-LHS,$1) :=$$ $(subst 
,$$!,$(subst #,$$&,$(subst $$,$$$$,$2))))$3
endef
define ^fset
$(and $(eval define $(call ~esc-LHS,$1)
$(subst \$ 
,\$$ 
,$(subst define,$$ define,$(subst endef,$$ endef,$2
)))endef
)1,$3)
endef
$(if ,, ) := 
~^es = $(if $(findstring $(if ,,,),$1),$$(if ,,$1),$1)
~^ed = $(if $(filter $1,$(words $2)),$(subst $  ,,$2),$(call ~^ed,$1,$2 $$))
define ^e
$(subst $$,$(if $2,$(call ~^ed,$2),$$),$(call ~^es,$(subst 
,$$!,$(subst $[,$$[,$(subst $],$$],$(subst $$,$$$$,$1))))))
endef
~promote = $(call ^u,$1)
~demote = $(call ^d,$1)
~nth = $(call ^n,$1,$2)
~set-global = $(call ^set,$1,$2,$3)
~set-rglobal = $(call ^fset,$1,$2,$3)
~apply = $(call ~^apply,$1,$2)
~*hooks* := 
~add-hook = $(call ^set,~*hooks*,$(~*hooks*) $1=$2)
~run-hooks = $(foreach funcname,$(patsubst $1=%,%,$(filter $1=%,$(~*hooks*))),$(call $(funcname)))
~^required-files := ///runtime.min
^require = $(foreach ^file,$(filter-out $(~^required-files),$(or $(word 1,$(foreach f,$(SCAM_MODS),$(if $(filter $(notdir $1),$(notdir $(basename $f))),$f))),$(if $(if $(filter-out u%,$(flavor ///$(notdir $1).min)),1),///$(notdir $1).min,$1.min))),$(and $(call ^set,~^required-files,$(~^required-files) $(^file))1,$(if $(findstring R,$(SCAM_DEBUG)),$(info require: $(^file)))1,$(if $(filter ///%,$(^file)),$(eval $(value $(^file))),$(eval include $(^file)))1,$(call ~run-hooks,load)1,$(if $(findstring Rx,$(SCAM_DEBUG)),$(info exited: $(^file)))))
define ~start
$(if $(if $(*started*),,1),$(and $(call ^set,*started*,1)1,$(if $(if $(filter-out u%,$(flavor ///trace.min)),1),$(call ^require,trace))1,$(call ^require,$(notdir $1))1,$(call ^Y,$(call $2,$3),,,,,,,,,$$(eval .DEFAULT_GOAL :=
.PHONY: .scam/-exit
.scam/-exit: $$(.DEFAULT_GOAL); @exit '$$(or $$(subst ',,$$(strip $$1)),0)'$$$$(call ~run-hooks,exit)))))
endef
$(if $(if $(if $(if $(filter-out u%,$(flavor ^start)),1),,1),$(call ^fset,^start,$(value ~start))),)
$(if $(if $(SCAM_MAIN),$(call ~start,$(SCAM_MAIN),,)),)

endef
 
define ///scam-ct.min
$(call ^require,runtime)
~when = L.56 S.57!0if $(call ^d,$(call ~nth,3,$1)) $(call ^d,L.62 S.63!0begin $(wordlist 4,99999999,$1))
~unless = L.118 S.119!0if $(call ^d,$(call ~nth,3,$1)) S.124!0nil $(call ^d,L.126 S.127!0begin $(wordlist 4,99999999,$1))

endef
 
define ///trace.min
~*trace-ignore-vars* := 
~*traces* := 
override SCAM_PRE := $(value SCAM_PRE)
^K = $(eval ^K_$0:=$(subst ioooooooooo,oi,$(^K_$0:o%=io%)o))
~trace-digits = $(if $(if $(findstring i,$1),,1),$(call ~trace-digits,i$1),$(if $(findstring ioooooooooo,$1),$(call ~trace-digits,$(subst ioooooooooo,oi,$1)),$(subst $  ,,$(wordlist $(words $(subst i, i,$1)),99,. . . . . . . . $(foreach d,$(subst i, i,$1),$(words $(subst i,,$(subst o, o,$d))))))))
~trace-n2a = $(if $(if $(filter i%,$1),,1),$(call ~trace-n2a,i$1),$(if $(findstring ioooooooooo,$1),$(call ~trace-n2a,$(subst ioooooooooo,oi,$1)),$(subst 10,A,$(words $(subst i, i,$1)))!0$(subst $  ,,$(foreach d,$(subst i, i,$1),$(words $(subst i,,$(subst o, o,$d)))))))
~list-of = $(if $(word $1,$2),$2,$(call ~list-of,$1,$2 x))
~trace-repeater = $(subst NAME,$1,$(subst N-1,$(wordlist 2,99999999,$(call ~list-of,$(or $2,11))),$(if $3,$$(if $$(^X),$$(call if,,,$$(value NAME)),$$(if $$(foreach ^X,N-1,$$(if $$(NAME),)),)$$(foreach ^X,0,$$(NAME))),$$(NAME)$$(if $$(foreach ^xx,N-1,$$(NAME)),))))
~trace-info = $(info TRACE: $1$2$3$4$5)
~trace-match-funcs = $(foreach v,$(if $(findstring %,$1),$(filter $1,$(filter-out $(~*trace-ignore-vars*),$(subst %,(),$(.VARIABLES)))),$1),$(if $(filter recur%,$(flavor $v)),$v))
~trace-instrument = $(if $(filter v,$1),$(and $(call ~trace-info,$2, [,$(flavor $2),] = ,$(value $2))1,$3),$(if $(filter c,$1),$$(^K)$3,$(if $(filter x% X%,$1),$(and $(call ~set-rglobal,$2~0~,$3)1,$(call ~trace-repeater,$2~0~,$(patsubst x%,%,$(subst X,x,$1)),$(filter X%,$1))),$(if $(filter p,$1),$(or $(SCAM_PRE),$(call ~trace-info,SCAM_PRE undefined; needed for ,$2,:p))$3,$(if $(filter t,$1),$(subst CODE,$3,$$(info --> ($$0$$(^ta)))$$(call ^tp,<-- $$0:,CODE)),$(and $(call ~trace-info,Unknown action: ',$1,')1,$3))))))
~*traces-active* := 
~trace-check = $(call ^set,~*traces-active*,$(strip $(~*traces-active*) $(foreach w,$(~*traces*),$(foreach name,$(call ~trace-match-funcs,$(firstword $(subst :, % ,$w))),$(foreach action,$(or $(wordlist 2,99999999,$(subst :, ,.$w)),t),$(if $(if $(filter $(name):$(patsubst x%,x,$(subst X,x,$(action))),$(~*traces-active*)),,1),$(and $(call ~set-rglobal,$(name),$(call ~trace-instrument,$(action),$(name),$(value $(name))))1,$(name):$(patsubst x%,x,$(subst X,x,$(action))))))))))
~trace-rev = $(if $1,$(call ~trace-rev,$(wordlist 2,99999999,$1)) $(firstword $1))
~trace-dump = $(and $(foreach s,$(foreach s,$(~*traces*),$(word 1,$(subst :, ,$s))),$(if $(if $(filter $s,$(foreach s,$(~*traces-active*),$(word 1,$(subst :, ,$s)))),,1),$(call ~trace-info,spec ',$s,' did not match any functions.)))1,$(if $(filter %c,$(~*traces-active*)),$(and $(call ~trace-info,function invocations)1,$(foreach r,$(call ~trace-rev,$(sort $(foreach V,$(filter ^K_%,$(.VARIABLES)),$(call ~trace-digits,$(value $V))$(patsubst ^K_%,::%,$V)))),$(call ~trace-info,$(subst ., ,$(word 1,$(subst ::, ,$r))), : ,$(word 2,$(subst ::, ,$r)))))))
~trace = $(and $(call ^set,~*traces*,$(~*traces*) $1)1,$(call ~trace-check))
~*trace-ignore-vars* := $(filter-out $(~*trace-ignore-vars*),$(subst %,(),$(.VARIABLES)))
$(if $(call ~trace,$(SCAM_TRACE)),)
$(if $(call ~add-hook,load,~trace-check),)
$(if $(call ~add-hook,exit,~trace-dump),)

endef
$(eval $(value ///runtime.min))
$(call ^start,///make,main,$(SCAM_ARGS))
