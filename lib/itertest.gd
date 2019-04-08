tool
extends EditorScript
const GL = GodotLINQ


class Human:
	var name: String
	var age: int
	var addr_id: int
	
	func _init(name, age, addr_id):
		self.name = name
		self.age = age
		self.addr_id = addr_id
		
func _run():
	run_test()
	
func test_func(val):
	print("called test with %s" % val)
	
# Called when the node enters the scene tree for the first time.
func run_test():
	var itr = GL.IterList.new([
	{name="mike", age=22, addr_id=0}, 
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=9, addr_id=2},
	{name="aslan", age=199, addr_id=1},
	Human.new("dan", 30, 0),
	Human.new("ariel", 65, 3),
	])
	
	var itr2 = GL.IterList.new([
	{addr_id=0, street="dalesford"}, 
	{addr_id=1, street="wendsleydale"},
	{addr_id=2, street="londonberry"},
	{addr_id=3, street="hubersville"},	
	])

#	var result = first.new({age=gt.new(70)}).eval(itr)
#	var result2 = first.new({addr_id=eq.new(result.addr_id)}).eval(itr2)

#	var sel_addr_id = select.new({age=gt.new(70)}, "addr_id")
#	var sel_addr_info = first.new({addr_id=select.new(eq, "addr_id")})
#	var result2 = connect(sel_addr_id, sel_addr_info).eval(items)
		
# for now, consider type checking inside gt, lt, eq
#	could alo just return query from 'select' and run() it to get items
# 	select.new({age=cmp_and.new([gt.new(20), lt.new(100)])}, ["addr_id", "name"])
#   select.run(itr)?

#	var result = itr.select({age=cmp_target.new(ai, gt)}, ["addr_id", "age"])
	var a = GL.cmp_func.new(funcref(self, "test_func"))
	var result_func = itr.first({name=a})
	
	var age_comp = GL.cmp_and.new([GL.gt.new(20), GL.lt.new(100)])
	var result = itr.select({age=age_comp}, ["addr_id", "name"])
#	print(result.to_list())
	
	for i in result.to_list():
		print(i.name, ", ", itr2.where({addr_id=GL.eq.new(i.addr_id)}).to_list())
	
#	var result = itr.first({age=gt.new(70)})
#	var result2 = itr2.first({addr_id=eq.new(result.addr_id)})
#	print(result,",",result2)	