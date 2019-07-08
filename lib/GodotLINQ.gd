#extends Resource
class_name GodotLINQ

class Comparer:
	func eval(item):
		return true
	
class has_field:
	extends Comparer
	var field
	var val

	func _init(field, val):
		self.field = field
		self.val = val

	func eval(item):
		if not field in item:
			return false
		return true

class and_:
	extends Comparer
	var cmps
	func _init(cmps):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if not c.eval(item):
				return false
		return true
	
class or_:
	extends Comparer
	var cmps
	func _init(cmps):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if c.eval(item):
				return true
		return false
	
class not_:
	extends Comparer
	var cmps
	func _init(cmps):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if c.eval(item):
				return false
		return true
	
class cmp_func:
	extends Comparer
	var func_ref
	func _init(func_ref):
		self.func_ref = func_ref
		
	func eval(item):
		return func_ref.call_func(item)
			
class cmp_func_args:
	extends Comparer
	var func_ref
	var args
	func _init(func_ref, args=null):
		self.func_ref = func_ref
		self.args = args
		
	func eval(item):
		return func_ref.call_func(args, item)
		
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
			
class any:
	extends Comparer

	func _init():
		pass
		
	func eval(item):
		return true
			
class in_:
	extends Comparer
	var items = []
	func _init(items):
		self.items = items
		
	func eval(item):
		return item in items
		
class contains:
	extends Comparer
	var item
	func _init(item):
		self.item = item
		
	func eval(item):
		return self.item in item