#extends Resource
class_name GodotLINQ

class Comparer:
	func eval(item):
		return true
	
class cmp_get:
	extends Comparer
	var target
	var val

	func _init(target, val):
		self.target = target
		self.val = val

	func eval(item):
		for c in target:
			if not c.eval(item):
				return false
		return true

class cmp_and:
	extends Comparer
	var cmps
	func _init(cmps):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if not c.eval(item):
				return false
		return true
	
class cmp_or:
	extends Comparer
	var cmps
	func _init(cmps):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if c.eval(item):
				return true
		return false
	
class cmp_func:
	extends Comparer
	var func_ref
	func _init(func_ref):
		self.func_ref = func_ref
		
	func eval(item):
		return func_ref.call_func(item)
			
class lt:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item < val
	
class gt:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item > val
			
class eq:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item == val
			

class IterList:
	var start 
	var curr
	var end
	var increment 
	var items = []

	func _init(items):
		increment = 1
		start = 0
		curr = start
		self.items = items
		self.end = items.size()
		
	func size():
		return items.size()

	func is_done():
		return (curr < end)

	func do_step():
		curr += increment
		return is_done()

	func _iter_init(arg):
		curr = start
		return is_done()

	func _iter_next(arg):
		return do_step()

	func _iter_get(arg):
		return items[curr]
		
	func where(cmps):
		var result = []
		for item in items:
			for key in cmps:
				if key in item:
					if cmps[key].eval(item[key]):
						result.append(item)
		return IterList.new(result)
		
	func select(cmps, fields: Array):
		var result = []
		for item in items:
			var n = {}
			for key in cmps:
				if key in item:
					for f in fields:
						if f in item\
						and cmps[key].eval(item[key]):
							n[f] = item[f]
			if not n.empty():
				result.append(n)
		return IterList.new(result)
		
	func first(cmps):
		var result = []
		for item in items:
			for key in cmps:
				if key in item:
					if cmps[key].eval(item[key]):
						return item
		return null
		
	func to_list(deep=false):
		return items.duplicate(deep)
