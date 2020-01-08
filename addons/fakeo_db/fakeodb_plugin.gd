tool
extends EditorPlugin

const LIB_NAME = 'FakeoDB'

func _enter_tree():
	self.add_autoload_singleton(LIB_NAME, "res://addons/fakeo_db/scripts/FakeoDB.gd")

func _exit_tree():
	self.remove_autoload_singleton(LIB_NAME)