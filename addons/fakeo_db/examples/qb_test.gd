tool
extends EditorScript

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory


"""
To use: File > Run
"""

# ==============================================================

class Human:
	var name: String
	var age: int
	var addr_id: int
	
	func _init(name, age, addr_id):
		self.name = name
		self.age = age
		self.addr_id = addr_id
	
class Address:
	var addr_id: int
	var street: String
	var value: int
	
	func _init(addr_id, street, value):
		self.addr_id = addr_id
		self.street = street
		self.value = value
		
var qry_root = fdb.qry().where("_item.age > 50").project(["name", "age"])
#var qry = 
#var qd = qry_root.where("_item.age > 50").project(["name", "age"])

func _run():
	var name_table = [
	{name="mike", age=22, addr_id=0},
	{name="mindy", age=16, addr_id=1},
	{name="trish", age=49, addr_id=2},
	{name="aslan", age=199, addr_id=1},
	Human.new("dan", 30, 2),
	Human.new("andy", 76, 2),
	Human.new("ariel", 65, 3),
	]
	
	var name_table2 = [
	{name="ellis", age=22, addr_id=0},
	{name="jane", age=52, addr_id=0},
	{name="arnor", age=16, addr_id=1},
	{name="tracy", age=409, addr_id=2},
	]	

	var addr_table = [
	{addr_id=0, street="vale road", value=20000},
	{addr_id=1, street="the lane", value=10000},
	{addr_id=2, street="london road", value=35250},
	Address.new(3, "diamond mews", 100000)
	]

	var q = fdb.qry(name_table).where({name="mike"}) # .project(["name"])	
	for i in q:
		print(i)

#	var dq = dfr_qry.duplicate(true) 
#	print(dq.source)
	qry_root.set_root(name_table)
	for i in qry_root:
		print(i)
#
	qry_root.set_root(name_table2)
	for i in qry_root:
		print(i)
