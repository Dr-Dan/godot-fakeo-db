class_name QueryBuilder_new

var start
var next

func _init():
	start = IterList_new.new()
	next = start
	
func project(fields):
	next = Enumerators_new.Project.new(fields, next)
	return self

func where(comps):
	next = Enumerators_new.Where.new(comps, next)
	return self

func take(amt):
	next = Enumerators_new.Take.new(amt, next)
	return self
		
		
func eval(data):
	start._init(data)
	return next


#func first(comps):
#	items.append(Enumerators.First.new(comps))
#	return get_script().new(items)		
#
#func last(comps):
#	items.append(Enumerators.Last.new(comps))
#	return get_script().new(items)		
#
#
#func at(index):
#	items.append(Enumerators.At.new(index))
#	return get_script().new(items)		
#
#
#func any(comps):
#	items.append(Enumerators.All.new(comps))
#	return get_script().new(items)					
#
#func all(comps):
#	items.append(Enumerators.Any.new(comps))
#	return get_script().new(items)				
#
#
#func count(comps):
#	items.append(Enumerators.Count.new(comps))
#	return get_script().new(items)		
#
#func values(field):
#	items.append(Enumerators.Values.new(field))
#	return get_script().new(items)		
	