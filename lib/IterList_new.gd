#extends Resource
class_name IterList_new

var start
var curr
var end
var increment
var items = []

func _init(items:Array=[]):
	increment = 1
	start = 0
	curr = start
	self.items = items
	self.end = items.size()

func should_continue():
	return (curr < end)

func _iter_init(arg):
	curr = start
	return should_continue()

func _iter_next(arg):
	curr += increment
	return should_continue()

func _iter_get(arg):
	return items[curr]
	
# =================================================================

# TODO: iterator shouldn't have size
func size():
	return items.size()

func empty():
	return size() == 0
	
func append(item):
	# if not item in items:
	items.append(item)

func append_many(items):
	for i in items:
		append(i)
		
func erase(item):
	if item in items:
		items.erase(item)
		
func erase_many(items):
	for i in items:
		erase(i)

func at(i: int):
	if i < items.size():
		return items[i]
	return null

# =================================================================

func where(cmps):
	return Enumerators_new.Where.new(cmps, self)
		
func project(fields: Array):
	return Enumerators_new.Project.new(fields, self)
	
func take(amt: int):
	return Enumerators_new.Take.new(amt, self)
		
func first(cmps):
	return Enumerators_new.First.new(cmps, self)
		
func to_list(deep=false):
	return items.duplicate(deep)
	
#func count(cmps) -> int:
#	var result = 0
#	for item in items:
#		for key in cmps:
#			if key in item:
#				if cmps[key].eval(item[key]):
#					result += 1
#	return result