extends Resource

class_name Builders

class Builder:
	func _init(): pass

	static func start() -> Chainer:
		return Chainer.new()

class Chainer:
	var items = []
	
	static func start() -> Chainer:
		return Chainer.new()
		
	func _init():
		pass
		
	func select(fields):
		items.append(Select.new(fields))
		return self
	
	func where(comps):
		items.append(Where.new(comps))
		return self


	func take(comps, amt):
		items.append(Take.new(comps, amt))
		return self		
		
	func first(comps):
		items.append(First.new(comps))
		return self

	func last(comps):
		items.append(Last.new(comps))
		return self
	

	func at(index):
		items.append(At.new(index))
		return self


	func any(comps):
		items.append(All.new(comps))
		return self			

	func all(comps):
		items.append(Any.new(comps))
		return self		
				

	func count(comps):
		items.append(Count.new(comps))
		return self
			
	func eval(data):
		var d = data
		for i in items:
			d = i.eval(d)
		return d

class Select:
	var fields = []

	func _init(fields):
		self.fields = fields

	func eval(items):
		var result = []
		for item in items:
			var n = {}
			for f in fields:
				n[f] = item[f]
			if not n.empty():
				result.append(n)
		return IterList.new(result)

class Where:
	var comps = []

	func _init(comps):
		self.comps = comps

	func eval(items):
		var result = []
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						result.append(item)
		return IterList.new(result)

class Take:
	var comps = []
	var amt = 0

	func _init(comps, amt):
		self.comps = comps
		self.amt = amt

	func eval(items):
		var result = []
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						result.append(item)
						if result.size() >= amt:
							return IterList.new(result)

		return IterList.new(result)
		
# TODO: return error. Null from Last/FirstOrDefault		
class First:
	var comps = []

	func _init(comps):
		self.comps = comps

	func eval(items):
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						return item
		return null 

class Last:
	var comps = []

	func _init(comps):
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

	func _init(index, default=null):
		self.comps = comps
		self.default = default

	func eval(items):
		if not items.empty() and index >= 0 and index < items.size():
			return items[index]
		return default

class Any:
	var comps = []

	func _init(comps):
		self.comps = comps

	func eval(items):
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						return true
		return false
		
class All:
	var comps = []

	func _init(comps):
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
	var comps = []

	func _init(comps):
		self.comps = comps

	func eval(items):
		var result = 0
		for item in items:
			for key in comps:
				if key in item:
					if comps[key].eval(item[key]):
						result += 1
		return result