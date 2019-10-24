

static func item_valid(item, comps: Dictionary):
	for key in comps:
		if key in item:
			if not comps[key].eval(item[key]):
				return false
	return true

	
class OperatorBase:
	func eval(item):
		return true
	
"""
	eval takes String
	returns true if field in eval item
"""
class HasField:
	extends OperatorBase
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
	extends OperatorBase
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
	extends OperatorBase
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
	extends OperatorBase
	var cmp
	func _init(cmp: OperatorBase):
		self.cmp = cmp
		
	func eval(item):
		if cmp.eval(item):
			return false
		return true

"""
	returns true for any item
"""
class Any:
	extends OperatorBase

	func _init():
		pass
		
	func eval(item):
		return true
	
"""
	returns false for any item
"""
class None:
	extends OperatorBase

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
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item < val
	
"""
	Greater than
"""
class GT:
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item > val
			
"""
	Equal to
"""
class Eq:
	extends OperatorBase
	var val
	func _init(val):
		self.val = val
		
	func eval(item):
		return item == val
					
# ------------------------------------------------------------ 
# LIST OPERATORS
class In:
	extends OperatorBase
	var container
	func _init(container):
		self.container = container
		
	func eval(item):
		for i in container:
			if item == i: return true
		return false
		
class Contains:
	extends OperatorBase
	var item
	func _init(item):
		self.item = item
		
	func eval(container):
		for i in container:
			if item == i: return true
		return false
#		return self.item in item

# ------------------------------------------------------------ 
# FUNCREF
"""
	
"""
class CmpFunction:
	extends OperatorBase
	var func_ref
	func _init(func_ref: FuncRef):
		self.func_ref = func_ref
		
	func eval(item):
		return func_ref.call_func(item)
			
class CmpFunctionWithArgs:
	extends OperatorBase
	var func_ref
	var args
	func _init(func_ref: FuncRef, args):
		self.func_ref = func_ref
		self.args = args
		
	func eval(item):
		return func_ref.call_func(item, args)
		
		