tool
extends EditorScript

const fdb = preload("res://addons/fakeo_db/scripts/FakeoDB.gd")
const ops = fdb.OperatorFactory

const Enumerables = fdb.Enumerables

"""
To use: File > Run
"""
	
func _run():
	var l = Enumerables.List.new([1,2,3,4,1,2])
	print(l.to_array())


	var l2 = Enumerables.List.new([
		{name="Dan", age=22, bag={food=5, matchbox={matches=20}}}, 
		{name="Sally", age=42, bag={food=13}}])

	var q3 = l2.project_deep(["name", "bag/matchbox/matches", "bag/food"])
	print(q3.to_array())

	var e = Enumerables.List.new([1,3,4])
	
	var qe2 = e.select_op(ops.expr('_item * 10'))
	print(qe2.to_array())
	
	var do_x = fdb.Operators.ExprArgsDict.new('x + y' , {x=2, y=4}).eval(null)
	print(do_x)
	
	var do_y = fdb.Operators.OpenMultiDeep.new(["bag/matchbox/matches", "name"]).eval(
		l2.at(0)
	)
	print("y: %s" % str(do_y))
	
	var get_food = ops.expr('{"name":_item.name, "food":_item.bag.food}')
	print(get_food.eval(
		{name="Sally", age=42, bag={food=22}}))
	print(l2.select_op(get_food).to_array())
	print(ops.expr('dmg*atk_rate', ["dmg", "atk_rate"]).eval(
		{name="Holy Spear", age=42, dmg=2, atk_rate=1.75}))
		
	var ll = fdb.cltn()
	l2.for_each_op(ops.expr('self.append(_item)', [], ll))
	ll.for_each_op(ops.expr('print(_item)'))
	
	print(ops.expr_dp('matches*food', ["bag/matchbox/matches", "bag/food"]).eval(l2.at(0)))
	print(ops.open_dp('bag/matchbox').eval(l2.at(0)))

# ==============================================================

func print_break():
	print("\n###############################")

func print_break_mini():
	print("\n--------------")

# ==============================================================
