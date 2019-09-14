class_name QueryBuilder

var items = []

func _init(items=[]):
	self.items = items
	
func select(fields):
	items.append(Enumerators.Select.new(fields))
	return get_script().new(items)

func where(comps):
	items.append(Enumerators.Where.new(comps))
	return get_script().new(items)		


func take(amt):
	items.append(Enumerators.Take.new(amt))
	return get_script().new(items)				
	
func first(comps):
	items.append(Enumerators.First.new(comps))
	return get_script().new(items)		

func last(comps):
	items.append(Enumerators.Last.new(comps))
	return get_script().new(items)		


func at(index):
	items.append(Enumerators.At.new(index))
	return get_script().new(items)		


func any(comps):
	items.append(Enumerators.All.new(comps))
	return get_script().new(items)					

func all(comps):
	items.append(Enumerators.Any.new(comps))
	return get_script().new(items)				
			

func count(comps):
	items.append(Enumerators.Count.new(comps))
	return get_script().new(items)		
		
func values(field):
	items.append(Enumerators.Values.new(field))
	return get_script().new(items)		
					
func eval(data):
	var d = data
	for i in items:
		d = i.eval(d)
	return d
