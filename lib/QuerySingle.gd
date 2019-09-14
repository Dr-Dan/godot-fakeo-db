class_name QuerySingle
	
var items = []

func _init(items=[]):
	self.items = items
	

func eval(data):
	var result = []
	# var d = data
	for d in data:
		for i in items:
			result.append(i.eval(d))
	return result
						
# func eval(data):
# 	var d = items[0].eval(data)
# 	for i in range(1, items.size()):
# 		d = items[i].eval(d)
# 	return d