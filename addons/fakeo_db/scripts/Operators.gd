

static func item_valid(item, comps: Dictionary):
	for key in comps:
		if key in item:
			if not comps[key].eval(item[key]):
				return false
		else:
			return false
	return true

	
class OperatorBase:
	func eval(item):
		return false
	
"""
	returns true if field in item. 
		i.e. 'item.field' exists
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
	returns true if all operators in 'cmps' are true
	Expects an array of operators

	usage:
		var comp = OpFac.And.new([OpFac.gt(20), OpFac.lt(70)]) # 20 < age < 70

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
	returns true if any operators in 'cmps' are true
	Expects an array of operators

	usage:
		var comp = OpFac.Or.new([OpFac.gt(20), OpFac.lt(70)]) # 20 < age < 70

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
	returns false if 'cmp' is true and vice versa
"""
class Not:
	extends OperatorBase
	var cmp
	func _init(cmp):
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
# handle dictionary value only

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
	Validate an item using a funcref.
		said function is expected to have an argument for the target item and return bool

	usage:
		func validate(item):
			...
			return true

		FuncOp.new(funcref(self, "validate"))	
"""
class FuncOp:
	extends OperatorBase
	var func_ref
	func _init(obj:Object, func_name:String):
		self.func_ref = funcref(obj, func_name)
		
	func eval(item):
		return func_ref.call_func(item)
		
"""
	Validate an item using a funcref
		said function is expected to have an argument for the target item and return bool
		takes up to 3 arguments as an array and the target function 

	usage:
		func validate(item, arg0, arg1):
			...
			return true

		FuncOpArgs.new(self, "validate", [arg0, arg1])
"""		
class FuncOpArgs:
	extends OperatorBase
	const MAX_ARGS = 3
	
	var func_ref
	var args
	var n_args:int
	
	func _init(obj:Object, func_name:String, args:Array):
		n_args = args.size()
		assert(n_args <= MAX_ARGS)		
		self.func_ref = funcref(obj, func_name)
		self.args = args
		
	func eval(item):
		match n_args:
			0:
				return func_ref.call_func(item)
			1:
				return func_ref.call_func(item, args[0])
			2:
				return func_ref.call_func(item, args[0], args[1])
			3:
				return func_ref.call_func(item, args[0], args[1], args[2])