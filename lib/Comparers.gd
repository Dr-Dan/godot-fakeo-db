#extends Resource
class_name Comparers

class Comparer:
	func eval(item):
		return true
	
"""
	eval takes String
	returns true if field in eval item
"""
class HasField:
	extends Comparer
	var field

	func _init(field: String):
		self.field = field

	func eval(item):
		if not field in item:
			return false
		return true

# ------------------------------------------------------------ 
# LOGICAL OPERATORS
"""
	returns true if all in 'cmps' are true
"""
class And:
	extends Comparer
	var cmps
	func _init(cmps:Array):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if not c.eval(item):
				return false
		return true
	
"""
	returns true if any in 'cmps' are true
"""
class Or:
	extends Comparer
	var cmps
	func _init(cmps:Array):
		self.cmps = cmps
		
	func eval(item):
		for c in cmps:
			if c.eval(item):
				return true
		return false
	
"""
	returns false if 'cmp'is true and vice versa
"""
class Not:
	extends Comparer
	var cmp
	func _init(cmp: Comparer):
		self.cmp = cmp
		
	func eval(item):
		if cmp.eval(item):
			return false
		return true

"""
	returns true for any item
"""
class Any:
	extends Comparer

	func _init():
		pass
		
	func eval(item):
		return true
	
"""
	returns true for any item
"""
class None:
	extends Comparer

	func _init():
		pass
		
	func eval(item):
		return false
# ------------------------------------------------------------ 
# COMPARITIVE OPERATORS

"""
	Less than
"""
class LT:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item < val
	
"""
	Greater than
"""
class GT:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item > val
			
"""
	Equal to
"""
class Eq:
	extends Comparer
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item == val
					
# ------------------------------------------------------------ 
# LIST OPERATORS
class In:
	extends Comparer
	var items = []
	func _init(items):
		self.items = items
		
	func eval(item):
		return item in items
		
class Contains:
	extends Comparer
	var item
	func _init(item):
		self.item = item
		
	func eval(item):
		return self.item in item

# ------------------------------------------------------------ 
# FUNCREF
"""
	
"""
class CmpFunction:
	extends Comparer
	var func_ref
	func _init(func_ref: FuncRef):
		self.func_ref = func_ref
		
	func eval(item):
		return func_ref.call_func(item)
			
class CmpFunctionWithArgs:
	extends Comparer
	var func_ref
	var args
	func _init(func_ref: FuncRef, args=null):
		self.func_ref = func_ref
		self.args = args
		
	func eval(item):
		return func_ref.call_func(args, item)
		
		