class Select:
	var fields = []

	func _init(fields: Array):
		self.fields = fields

	func eval(items):
		var result = []
		for item in items:
			var n = {}
			for f in fields:
				if f in item:
					n[f] = item[f]
			if not n.empty():
				result.append(n)
		return result

class Where:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		var result = []
		for item in items:
			var valid = true
			for key in comps:
				if key in item:
					if not comps[key].eval(item[key]):
						valid = false
						break
			if valid:
				result.append(item)
		return result

class Take:
	var amt = 0

	func _init(amt: int):
		self.amt = amt

	func eval(items):
		var result = []
		var n_items = items.size()
		var n = min(amt, n_items)
		if n > 0 and n <= n_items:
			for i in range(n):
				result.append(items[i])

		return result
		
# TODO: add default argument to _init
class First:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						return item
		return null 

class Last:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		var itm = null
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						itm = item
		return itm
		
		
class At:
	var index = -1
	var default  = null

	func _init(index: int, default=null):
		self.index = index
		self.default = default

	func eval(items):
		if index >= 0 and index < items.size():
			return items[index]
		return default

class Any:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						return true
		return false
		
class All:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		if items.empty():
			return false

		for item in items:
			for key in comps:
				if key in item:
					if not comps[key].eval(item[key]):
						return false
		return true

		
class Count:
	var comps = {}

	func _init(comps: Dictionary):
		self.comps = comps

	func eval(items):
		var result = 0
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						result += 1
		return result

class Values:
	var field
	func _init(field):
		self.field = field
		
	func eval(items):
		var result = []
		for item in items:
			if field in item:
				result.append(item[field])
		return result
	