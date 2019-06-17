#!/bin/bash
:; for v in "${@//!/!1}" ; do v=${v// /!0} ; v=${v//	/!+}; a[++n]=${v:-!.} ; done ; LC_ALL=C SCAM_ARGS=${a[*]} exec make -Rr --no-print-directory -f"$0" 9>&1
SCAM_MAIN := ./.scam/485d4eb883a2f366/make.scm:'main
^uid := b2f923aa4d148147

define [mod-./.scam/485d4eb883a2f366/make.scm]
$(call ^R,core)
'top := top
'linked-dirs := .emacs.d .config
'vec-relpath = $(if $(and $1,$(call `eq?,$(call ^n,1,$1),$(call ^n,1,$2))),$(call 'vec-relpath,$(wordlist 2,99999999,$1),$(wordlist 2,99999999,$2)),$(call `append,$(foreach ;,$1,..),$2))
'relpath = $(call `concat-vec,$(call 'vec-relpath,$(call `split,/,$(abspath $1)),$(call `split,/,$(abspath $2))),/)
'find-files = $(foreach ;,$1,$(call ^Y,$(wildcard $;/.* $;/*),,,,,,,,,$`(if $`1,$`(call 'find-files,$`(filter-out %/.. %/.,$`1)),$(call ^E,$;))))
'is-symlink? = $(shell if [ -L '$1' ] ; then echo 1 ; fi)
'help-raw := Usage:$'   make help         Display this message$'   make install      Install symbolic links$'   make uninstall    Remove symbolic links$'$'The `top` directory contains an image of files to be propagated to your$'$`HOME directory.$'$'Instead of copying the files, `make install` creates symbolic links so that$'this repository can be easily updated using git.  `make install` avoids$'removing files or links that already exist; you may have to manually remove$'some files in order to deploy the ones in this project.  Also, you can$'manually replace a symbolic link with a copy in order to maintain the local$'$`HOME directory in a state that diverges from the project (e.g. work$'machines vs. personal machines).$'$'In some cases, symbolic links are created to directories rather than$'individual files.  This can make it easier to track files that are added to$'those directories and propagate changes back into this project.  Linked$'directories are: LDIRS$'$'Modify the `linked-dirs` variable in make.scm to change this.$'
'install-file = $(and $(if $(if $(wildcard $(dir $1)),,1),$(and $(info Creating directory: $(dir $1))1,$(shell mkdir -p $(dir $1))))1,$(if $(wildcard $1),$(if $(call 'is-symlink?,$1),$(info Skipping $2 (already a symlink)),$(info AVOIDING $2 (remove pre-existing file first))),$(and $(shell ln -fs $(call 'relpath,$(dir $1),$('top))/$2 $1)1,$(info Installed $2))))
'uninstall-file = $(if $(call 'is-symlink?,$1),$(and $(info Removing $1)1,$(shell rm $1)),$(if $(wildcard $1),$(info LEAVING $1 (not a symlink)),$(info Missing $1)))
'visit-files = $(foreach ;,$(call `append,$(filter-out $(addsuffix /%,$('linked-dirs)),$(patsubst $('top)/%,%,$(call 'find-files,$('top)))),$('linked-dirs)),$(call ^Y,$('HOME)/$;,$;,,,,,,,,$1))
'rules := help show install uninstall
define 'build
$(if $(call `eq?,$1,help),$(info $(subst LDIRS,$(foreach ;,$('linked-dirs),$(subst D,$;,
   ~/D/)),$('help-raw))),$(if $(call `eq?,$1,show),$(call 'visit-files,$`(info $`1 --> $`2)),$(if $(call `eq?,$1,install),$(call 'visit-files,$(value 'install-file)),$(if $(call `eq?,$1,uninstall),$(call 'visit-files,$(value 'uninstall-file))))))
endef
define 'main
$(and $(foreach ;,$('rules),$(eval $(subst X,$;,.PHONY: X
X: ; @true $`(call 'build,X)
)))1,$(eval Makefile: make.scm; @top/local/bin/scam -o $`@ $`<))
endef

endef

define [mod-runtime]
`*do-not-trace* := $(.VARIABLES)
define '


endef
 [ := (
 ] := )
" := \#
' := $'
` := $$
& := ,
$(if ,, ) :=

^d = $(or $(subst $  ,!0,$(subst $ 	,!+,$(subst !,!1,$1))),!.)
^u = $(subst !1,!,$(subst !+,	,$(subst !0, ,$(subst !.,,$1))))
^n = $(subst !1,!,$(subst !+,	,$(subst !0, ,$(subst !.,,$(word $1,$2)))))
^k = $(subst %,!8,$(^d))
^Y = $(call if,,,$(10))
^v = $(subst !.,!. ,$(filter-out %!,$(subst !. ,!.,$(foreach n,$(wordlist $N,9,1 2 3 4 5 6 7 8),$(call ^d,$($n)))$(if $9, $9) !)))
^av = $(foreach N,1,$(^v))
^apply = $(call ^Y,$(call ^n,1,$2),$(call ^n,2,$2),$(call ^n,3,$2),$(call ^n,4,$2),$(call ^n,5,$2),$(call ^n,6,$2),$(call ^n,7,$2),$(call ^n,8,$2),$(wordlist 9,99999999,$2),$1)
^na = $(call if,,,$`(call $1$(subst $(if ,, ,),$(if ,,,),$(foreach ;,$(wordlist 1,$(words $2),1 2 3 4 5 6 7 8),$(if ,,,$`(call ^n,$;,$`2))))$(if $(word 9,$2),$(if ,,,$`(wordlist 9,99999999,$`2)))))
define ^f
"$(subst 
,\n,$(subst ",\",$(subst \,\\,$1)))"
endef
^tp = $(info $1 $(call ^f,$2))$2
^tc = $(call $1,$2,$3,$4,$5,$6,$7,$8,$(call ^n,1,$9),$(wordlist 2,9999,$9))
^ta = $(if $(or $1,$2,$3,$4,$5,$6,$7,$8,$9), $(^f)$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9))
^t = $(info --> ($1$(call ^tc,^ta,$2,$3,$4,$5,$6,$7,$8,$9)))$(call ^tp,<-- $1:,$(call ^tc,$1,$2,$3,$4,$5,$6,$7,$8,$9))
define `esc-LHS
$`(if ,,$(subst $],$`],$(subst $[,$`[,$(subst 
,$`',$(subst #,$`",$(subst $`,$`$`,$1))))))
endef
define ^set
$(eval $(call `esc-LHS,$1) :=$` $(subst 
,$`',$(subst #,$`",$(subst $`,$`$`,$2))))$3
endef
define ^fset
$(and $(eval define $(call `esc-LHS,$1)
$(subst \$ 
,\$` 
,$(subst define,$` define,$(subst endef,$` endef,$2
)))endef
)1,$3)
endef
define ^E
$(subst $`,$`$2,$`(if ,,$(subst 
,$`',$(subst $[,$`[,$(subst $],$`],$(subst $`,$``,$1))))))
endef
`filtersub = $(patsubst $1,$2,$(filter $1,$3))
^tags := 
^at = $(call ^set,^tags,$(^tags) $(filter-out $(^tags),$1))
`*required* := 
^load = $(and $(if $(if $(filter-out u%,$(flavor [mod-$1])),1),$(eval $(value [mod-$1])),$(eval include $(value SCAM_DIR)$1.o))1,$1)
^R = $(and $(or $(filter $1,$(`*required*)),$(and $(call ^set,`*required*,$(`*required*) $1)1,$(call ^load,$1)))1,)
`trace-info = $(info TRACE: $1$2$3$4)
`trace-digits = $(if $(findstring /1111111111,$1),$(call `trace-digits,$(subst /1111111111,1/,$1)),$(subst !:,,$(subst :!,0!,$(subst :0,::,$(subst :00,:::,$(subst :0000,:::::,$(subst $  ,,!:$(foreach ;,/$(subst /, /,$1),$(words $(subst /,,$(subst 1, 1,$;))))!:)))))))
`trace-words = $(if $(word $1,$2),$2,$(call `trace-words,$1,1 $2))
`trace-body = $(subst :D,$4,$(subst :N,$(patsubst '%,%,$2),$(subst :C,$`(call [S-$3],$`1,$`2,$`3,$`4,$`5,$`6,$`7,$`8,$`9),$(subst :E,$`(eval ^TI:=$`$`(^TI) ):C$`(eval ^TI:=$`$`(subst x ,,x$`$`(^TI))),$(subst :I,info $`(^TI),$(if $(filter c,$1),$(and $(call ^set,[K-$3],$(or $(value [K-$3]),///////),)1,$`(eval [K-$3]:=$`(subst /1111111111,1/,$`([K-$3])1)):D),$(if $(filter p%,$1),$(subst :,:$` ,$(call ^u,$(patsubst p%,%,$1))):D,$(if $(filter t f,$1),$(if $(or $(filter f,$1),$(filter ^ta ^f ^tc ^tp ^n,$2)),$`(:I--> :N):E$`(:I<-- :N),$`(:I--> (:N$`(^ta)))$`(call ^tp,$`(^TI)<-- :N:,:E)),$(if $(filter x%,$1),$`(foreach ^X,1,:C)$`(if $`(^X),,$`(if $`(foreach ^X,$(wordlist 2,99999999,$(call `trace-words,$(or $(patsubst x%,%,$1),11),1)),$`(if :C,)),)),$(error TRACE: Unknown mode: '$1'))))))))))
`trace-match = $(foreach ;,$(if $(filter '% `% "%,$1),$(patsubst "%,%,$1),'$1),$(filter-out $(foreach ;;,^% `% `trace% `esc-% `set-native-fn `filtersub,$(if $(filter-out $(;;),$;),$(;;))),$(filter $;,$2)))
`*trace-ids* := 
`trace-id = $(or $(call `filtersub,$1:%,%,$(`*trace-ids*)),$(if $2,$(call ^set,`*trace-ids*,$(`*trace-ids*) $1:$(words $(`*trace-ids*)),$(words $(`*trace-ids*)))))
define `trace
$(subst "`,`,$(subst "',',$(addprefix ",$(filter %,$(foreach ;,$(filter-out %:v %:-,$1),$(foreach ;;,$(foreach ;;,$(call `trace-match,$(filter-out :%,$(subst :, :,$;)),$(filter-out $(`*do-not-trace*) [% ~trace ~untrace ~trace-ext ~untrace-ext ^Y  $(call `filtersub,%:-,%,$1),$(.VARIABLES))),$(if $(filter filerec%,$(origin $(;;))$(flavor $(;;))),$(;;))),$(foreach ;;;,$(call `trace-id,$(;;),1),$(and $(and $(if $(filter u%,$(origin [S-$(;;;)])),$(call ^fset,[S-$(;;;)],$(value $(;;)),))1,$(if $(filter %:v,$1),$(call `trace-info,[,$(subst $  ,,$(wordlist 2,999,$(subst :,: ,$;))),] ,$(;;)))1,$(call ^fset,$(;;),$(call `trace-body,$(or $(subst $  ,,$(wordlist 2,999,$(subst :,: ,$;))),t),$(subst #,$`",$(;;)),$(;;;),$(value [S-$(;;;)])),))1,$(;;)))))))))
endef
`trace-rev = $(if $1,$(call `trace-rev,$(wordlist 2,99999,$1)) $(word 1,$1))
`trace-dump = $(foreach ;,$(sort $(foreach ;,$1,$(foreach ;;,$(value [K-$(call `trace-id,$;)]),$(if $(findstring 1,$(;;)),$(and $(call ^set,[K-$(call `trace-id,$;)],///////,)1,$(call ^d,$(subst :, ,$(call `trace-digits,$(;;))) $;)))))),$(call ^d,$(call `trace-info,$(call ^u,$;))))
`untrace = $(and $(call `trace-dump,$(foreach ;,$(filter $1,$(filter-out :%,$(subst :, :,$(`*trace-ids*)))),$(foreach ;;,$(call `trace-id,$;),$(and $(call ^fset,$;,$(value [S-$(;;)]),)1,$;))))1,$2)
`do-not-trace = $(call ^set,`*do-not-trace*,$(`*do-not-trace*) $1)
`start-trace = $(call `trace,$(value $(if $(filter scam,$1),_)SCAM_TRACE))
SHELL := /bin/bash
`*atexits* := 
`at-exit = $(if $(and $2,$(findstring $  $(call ^d,$1) , $(`*atexits*) )),,$(call ^set,`*atexits*,$(call ^d,$1) $(`*atexits*)))
`run-at-exits = $(and $(foreach ;,$(`*atexits*),$(call ^d,$(call ^Y,,,,,,,,,,$(call ^u,$;))))1,)
`check-exit = $(if $(subst 0,,$(subst 9,,$(subst 8,,$(subst 7,,$(subst 6,,$(subst 5,,$(subst 4,,$(subst 3,,$(subst 2,,$(subst 1,,$(patsubst -%,%,$(subst $ 	,x,$(subst $  ,x,$1))))))))))))),$(error scam: main returned '$1'),$(or $1,0))
define ^start
$(and $(call `start-trace,$1)1,$(call `do-not-trace,^R ^load)1,$(call ^R,$1)1,$(call `start-trace,$1)1,$(eval $(call ^Y,$(call `check-exit,$(call ^Y,$3,,,,,,,,,$(value $2))),,,,,,,,,.DEFAULT_GOAL :=
.PHONY: [exit]
[exit]: $`(.DEFAULT_GOAL);@exit $`1$``(call `run-at-exits))))
endef
$(if $(call `do-not-trace,^start `start-trace),)
$(if $(call `at-exit,$`(call `trace-dump,$`(filter-out :%,$`(subst :, :,$`(`*trace-ids*))))),)
$(if $(call ^start,$(word 1,$(subst :, ,$(SCAM_MAIN))),$(word 2,$(subst :, ,$(SCAM_MAIN))),$(value SCAM_ARGS)),)

endef

define [mod-core]
`eq? = $(if $(findstring 1$1,$(findstring 1$2,1$1)),1)
`xor = $(if $1,$(if $2,,$1),$2)
`concat-vec = $(call ^u,$(subst $  ,$(call ^d,$2),$1))
`cons = $(call ^d,$1)$(if $2, )$2
`conj = $1$(if $1, )$(call ^d,$2)
`butlast = $(wordlist 2,$(words $1),X $1)
`select-vec = $(filter-out !,$(foreach ;,$2,$(if $(call ^Y,$(call ^u,$;),,,,,,,,,$1),$;,!)))
`select-words = $(foreach ;,$(foreach ;,$2,$(if $(call ^Y,$;,,,,,,,,,$1),$;)),$;)
`vec-filter = $(if $(findstring %,$2),$(subst !P,%,$(call $1,$(subst %,!P,$2),$(subst %,!P,$3))),$(call $1,$2,$3))
`indices-b = $(if $(filter $1,$2),$2,$2 $(call `indices-b,$1,$(words $3),. $3))
`indices-a = $(if $(filter-out 0,$1),$(call `indices-b,$1,1,. .))
`rev-by-10s = $(if $1,$(if $2,$(foreach ;,10 9 8 7 6 5 4 3 2 1,$(call `rev-by-10s,$(wordlist $(word $;,0 1 2 3 4 5 6 7 8 9)$(patsubst %0,%1,$2),$;$2,$1),$(patsubst 0%,%,$2))),$(foreach ;,10 9 8 7 6 5 4 3 2 1,$(word $;,$1))))
`rev-zeroes = $(if $(word 1$21,$1),$(call `rev-zeroes,$1,0$2),$2)
`reverse = $(wordlist 1,99999999,$(call `rev-by-10s,$1,$(call `rev-zeroes,$1,)))
`while-0 = $(if $(filter iiiiiiiiiiiiiiiiiiii,$4),1 $(call ^d,$3),$(if $(call ^Y,$3,,,,,,,,,$1),$(call `while-0,$1,$2,$(call ^Y,$3,,,,,,,,,$2),i$4),0 $(call ^d,$3)))
`while-N = $(if $(filter 0,$(word 1,$3)),$3,$(if $(filter iii,$5),$(if $(filter 1,$4),$(call `while-N,$1,$2,$3,$4 0,ii),$3),$(call `while-N,$1,$2,$(if $4,$(call `while-N,$1,$2,$3,$(wordlist 2,99999999,$4),),$(call `while-0,$1,$2,$(call ^n,2,$3),)),$4,i$5)))
`while = $(if $(call ^Y,$3,,,,,,,,,$1),$(call ^Y,$(call ^Y,$3,,,,,,,,,$2),$1,$2,,,,,,,$`(call ^n,2,$`(call `while-N,$`2,$`3,$`(call `while-0,$`2,$`3,$`1,),1,ii))),$3)
`numeric? = $(if $(filter 0% 1% 2% 3% 4% 5% 6% 7% 8% 9%,$(subst -,,$1)),$(if $(patsubst .%,%,$(patsubst %e,%,$(subst 0,,$(patsubst -%,%,$(subst $  ,_,$(subst E0,e,$(subst E-,E,$(subst e,E,$(subst +,-,$(subst 9,0,$(subst 8,0,$(subst 7,0,$(subst 6,0,$(subst 5,0,$(subst 4,0,$(subst 3,0,$(subst 2,0,$(subst 1,0,$1)))))))))))))))))),,$1))
`word-index? = $(if $(subst 9,,$(subst 8,,$(subst 7,,$(subst 6,,$(subst 5,,$(subst 4,,$(subst 3,,$(subst 2,,$(subst 1,,$(subst 0,,$1)))))))))),,$(subst 0,,$1))
`append = $(filter %,$1 $2 $3 $4 $5 $6 $7 $8 $(if $9,$(call ^u,$9)))
`dict-key = $(call ^u,$(subst !8,%,$(word 1,$(subst !=, ,$1))))
`dict-find = $(word 1,$(filter $(subst %,!8,$(call ^d,$1))!=%,$2))
`dict-get = $(call ^u,$(or $(word 2,$(subst !=, ,$(filter $(subst %,!8,$(call ^d,$1))!=%,$2))),$(subst !,!1,$3)))
`dict-remove = $(filter-out $(subst %,!8,$(call ^d,$1))!=%,$2)
`dict-set = $(foreach ;,$(subst %,!8,$(call ^d,$1))!=,$;$(call ^d,$2) $(filter-out $;%,$3))
`dict-compact = $(if $(if $1,,1),$2,$(call `append,$(word 1,$1),$(call `dict-compact,$(filter-out $(word 1,$(subst !=,!=% ,$(word 1,$1))),$(wordlist 2,99999999,$1)))))
`dict-keys = $(subst !8,%,$(filter-out !=%,$(subst !=, !=,$1)))
`dict-values = $(filter-out %!=,$(subst !=,!= ,$1))
`dict-collate = $(foreach ;,$(word 1,$(subst !=,!= ,$(word 1,$1))),$(call `append,$;$(call ^d,$(call `filtersub,$;%,%,$1)),$(call `dict-collate,$(filter-out $;%,$1))))
define `symbol?
$(and $(findstring $1,$(word 1,$1)),$(if $(or $(findstring 
,$1),$(findstring $[,$1),$(findstring $],$1),$(findstring [,$1),$(findstring ],$1),$(findstring $(if ,,,),$1),$(findstring ;,$1),$(findstring :,$1),$(findstring ',$1),$(findstring `,$1),$(findstring ",$1),$(findstring !=,$1)),,1),$1)
endef
`format-dict = $(if $(findstring !=,$1),$(if $(call `eq?,$1,$(foreach ;,$1,$(call ^k,$(call ^n,1,$(subst !=, ,$;)))!=$(call ^d,$(call ^n,2,$(subst !=, ,$;))))),{$(call `concat-vec,$(foreach ;,$1,$(call ^d,$(or $(call `symbol?,$(call `dict-key,$;)),$(call `format,$(call `dict-key,$;))): $(call `format,$(call ^n,2,$(subst !=, ,$;))))),$(if ,,, ))}))
`data-foreach = $(if $2,$(call `data-foreach,$1,$(wordlist 2,99999999,$2),$(wordlist 2,99999999,$3),$4$(if $4, )$(call ^Y,$(if $(filter L,$(word 1,$2)),$3,$(if $(filter S,$(word 1,$2)),$(call ^n,1,$3),$(if $(filter W,$(word 1,$2)),$(word 1,$3),$(error bad encoding in ctor pattern)))),$(word 1,$2),,,,,,,,$1)),$4)
`format-record = $(if $(filter !:%,$(word 1,$1)),$(call ^Y,$(call `dict-get,$(word 1,$1),$(^tags)),$(wordlist 2,99999999,$1),$(word 1,$1),$1,,,,,,$`(and $`1,$`(call `eq?,$`(filter %,$`4),$`(filter %,$`(call `data-foreach,$``(if $``(call `eq?,S,$``2),$``(call ^d,$``1),$``1),$`(wordlist 2,99999999,$`1),$`2,$`3))),($`(call ^n,1,$`1)$`(if $`(wordlist 2,99999999,$`1), )$`(call `data-foreach,$``(if $``(and $``(call `eq?,L,$``2),$``(if $``1,,1)),[],$``(call `format,$``1)),$`(wordlist 2,99999999,$`1),$`2,)))))
`*format-funcs* := 
`format-add = $(call ^set,`*format-funcs*,$(call `cons,$1,$(`*format-funcs*)))
`format-custom = $(if $2,$(or $(call ^Y,$1,,,,,,,,,$(call ^n,1,$2)),$(call `format-custom,$1,$(wordlist 2,99999999,$2))))
define `format
$(or $(call `format-custom,$1,$(`*format-funcs*)),$(if $(findstring !,$1),$(or $(call `format-dict,$1),$(call `format-record,$1))),$(if $(or $(findstring !,$1),$(and $(findstring $  ,$1),$(call `numeric?,$(subst $  ,,$1)))),$(if $(call `eq?,$1,$(foreach ;,$1,$(call ^d,$(call ^u,$;)))),[$(foreach ;,$1,$(call `format,$(call ^u,$;)))])),$(call `numeric?,$1),"$(subst ,\x0d,$(subst $ 	,\t,$(subst 
,\n,$(subst ",\",$(subst \,\\,$1)))))")
endef
`vsp-split = $(subst !%$(or $(word 1,$1),%),!:$(word 1,$1) ,$(subst !%$(or $(word 2,$1),%),!:$(word 2,$1) ,$(if $(word 3,$1),$(call `vsp-split,$(wordlist 3,99999999,$1),$2),$2)))
`vsprintfx = $(call `concat-vec,$(foreach ;,$(join $(subst !%,%,$(call `vsp-split,$3,$(subst !%!%,%,$(subst %,!%,$(call ^d,$1))))),$(addprefix !:%,$2)),$(word 1,$(subst !:, !. ,$;))$(if $(findstring !:,$(subst !:%,,$;)),$(call ^Y,$(word 2,$(subst !:, ,x$;)),$(word 2,$(subst !:%, ,$;)),,,,,,,,$4))))
`vsprintf = $(call `vsprintfx,$1,$2,s q,$`(if $`(filter q,$`1),$`(call ^d,$`(call `format,$`(call ^u,$`2))),$`2))
`sprintf = $(call `vsprintf,$1,$(foreach N,2,$(^v)))
`printf = $(info $(call `vsprintf,$1,$(foreach N,2,$(^v))))
define `expect-x
$(if $(call `eq?,$1,$2),$(if $(findstring O,$(SCAM_DEBUG)),$(info $3: OK: $1)),$(and $(info $3: error: assertion failed
A: $(call `format,$1)
B: $(call `format,$2)

Raw:
A: $1
B: $2
)1,$(if $(findstring K,$(SCAM_DEBUG)),$(call `at-exit,$`(error $`1),1),$(error ))))
endef
`assert-x = $(or $1,$(error $(info $2: error: assertion failed)))
define `see
$(if $(findstring $1,$2),1,$(and $(info Expected: $(subst 
,
          ,$1))1,$(info $   Within: $(subst 
,
          ,$2))))
endef
`uniq-x = $(if $1,$(word 1,$1) $(call `uniq-x,$(filter-out $(word 1,$1),$(wordlist 2,99999999,$1))))
`uniq = $(subst ~1,~,$(subst ~p,%,$(filter %,$(call `uniq-x,$(subst %,~p,$(subst ~,~1,$1))))))
`split = $(subst !x,,$(patsubst !x,!.,!x$(subst $ 	,!+,$(subst !!,!1,$(subst $  ! ,!0,$(subst $(subst $  , ! ,$(subst !,!!,$1)), !x,$(subst $  , ! ,$(subst !,!!,$2))))))))
`1+ = $(if $(filter %1 %2 %3 %4,$1),$(subst 1~,2,$(subst 2~,3,$(subst 3~,4,$(subst 4~,5,$1~)))),$(if $(filter %5 %6 %7,$1),$(subst 5~,6,$(subst 6~,7,$(subst 7~,8,$1~))),$(if $(findstring 9~,$1~),$(call `1+,$(or $(subst 9~,,$1~),0))0,$(patsubst %0,%1,$(patsubst %8,%9,$1)))))
`mcache = $(and $(if $6,$(info Warning: memoized function passed more than three arguments))1,$(if $(if $(if $(filter-out u%,$(flavor $1)),1),,1),$(call ^set,$1,$(call ^Y,$3,$4,$5,,,,,,,$2),))1,$($1))
`memoenc = $(if $(or $1,$2,$3),~~$(subst ~,~0,$1)$(call `memoenc,$2,$3))
`memoize = $(if $(if $(if $(filter-out u%,$(flavor $1)),1),,1),$(info Warning: [memoize-1] function '$1' not defined.),$(call ^Y,$(value $1),*memo$(call `memoenc,$1),$1,,,,,,,$`(call ^fset,$`3,$``(call `mcache,$`(call ^E,$`2)$``(call `memoenc,$``1,$``2,$``3),$`(call ^E,$`1),$``1,$``2,$``3,$``(or $``4,$``5,$``6,$``7,$``8)),)))
`sort-by = $(filter-out %!!,$(subst !!,!! ,$(sort $(foreach ;,$2,$(call ^d,$(call ^Y,$(call ^u,$;),,,,,,,,,$1))!!$;))))
`assoc-initial = $(call ^u,$(firstword $(if $(findstring %,$1),$(subst !8,$1,$(filter !8 !8!0%,$(subst $1,!8,$2))),$(filter $1 $1!0%,$2))))
`index-of = $(words $(subst !_, ,$(filter %!|,$(subst !_$(call ^d,$2)!_,!_!| ,!_$(subst $  ,!_,$1)!_))))
`foldl = $(if $(word 1,$3),$(call `foldl,$1,$(call ^Y,$2,$(call ^n,1,$3),,,,,,,,$1),$(wordlist 2,99999999,$3)),$2)
`foldr = $(if $(word 1,$3),$(call ^Y,$(call ^n,1,$3),$(call `foldr,$1,$2,$(wordlist 2,99999999,$3)),,,,,,,,$1),$2)
`intersperse = $(subst $  , $(call ^d,$1) ,$2)
`repeat-words = $(if $(filter-out -% 0,$2),$(if $(word $2,$1 $1 $1),$(wordlist 1,$2,$1 $1 $1),$(call `repeat-words,$1 $1 $1,$2)))

endef
$(eval $(value [mod-runtime]))
