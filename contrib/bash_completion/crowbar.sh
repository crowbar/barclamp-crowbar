# bash completion for crowbar

_crowbar_opts="" # lazy init
_crowbar_opts_exp="" # lazy init

_crowbar_pro_opts="create commit edit delete list show dequeue"

# dict hack for bash 3
_set_crowbar_subopts () {
  eval _crowbar_subopts_"$1"='$2'
}
_get_crowbar_subopts () {
  eval echo '${_crowbar_subopts_'"$1"'#_crowbar_subopts_}'
}

_crowbar()
{
	local cur prev subopts
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	if [[ $COMP_CWORD -gt 1 ]] ; then
		pprev="${COMP_WORDS[COMP_CWORD-2]}"
	else
		pprev=""
	fi

	if [ "x$_crowbar_opts" == "x" ] ; then
		_crowbar_opts="`crowbar | tail -n 1 | sed -e 's/^.*: \(.*\)$/\1/'`"
		_crowbar_opts_exp="`echo $_crowbar_opts | sed -e "s/\s/|/g"`"
	fi

	if [[ " `echo $_crowbar_opts` " =~ " $prev " ]] && [[ "$pprev" == "crowbar" ]] ; then
		if [ "x$(_get_crowbar_subopts "$prev")" == "x" ] ; then
			subopts=`crowbar $prev | grep -v "API help" | sed  -e '1d' -e 's/^  \([^ ]*\).*$/\1/' -e '/^$/d'`
			_set_crowbar_subopts "$prev" "$subopts"
		fi
		COMPREPLY=($(compgen -W "$(_get_crowbar_subopts "$prev")" -- ${cur}))
	elif [[ "$prev" == "proposal" ]] ; then
		COMPREPLY=($(compgen -W "$_crowbar_pro_opts" -- ${cur}))
	elif [[ " $_crowbar_pro_opts " =~ " $prev " ]] ; then
		COMPREPLY=($(compgen -W "" -- ${cur}))
	elif [[ ! " ${COMP_WORDS[@]} " =~ " "($_crowbar_opts_exp)" " ]] || [[ "$prev" == "crowbar" ]] ; then
		COMPREPLY=($(compgen -W "${_crowbar_opts}" -- ${cur}))  
	fi
	return 0
}
complete -F _crowbar crowbar
