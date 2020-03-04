tool
extends EditorScript

func _run() -> void:
#	var r = RegEx.new()
#	r.compile("[^\\(|\\)$]")
#	var result = r.search_all("(0.2,0)")
#	for i in result:
#		print(i.get_string())
#	print("(0,0)".match("(*,*)"))
	var s = "(0,2,0.34)"
	var ss = s.substr(1, s.length()-2).split(",")
	print(ss[0].is_valid_float())
#	print(s.substr(1, s.length()-2).split_floats(","))
