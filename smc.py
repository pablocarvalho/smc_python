import ply.lex as lex
import ply.yacc as yacc
import os
import numbers
import node as nd


class SMCmachine:
	def __init__(self):         
         self.s = Stack()
         self.m = []
         self.c = Stack()

	def print_state(self):
		state = '<'+ str(self.s.items) + 'S,' + str(self.m) + 'M,' + str(self.c.items) + 'C>'
		print state


	def load_program(self, current_node):

		if(isinstance(current_node,nd.Node)):
			if(current_node.type == 'binop'):
				self.c.push(current_node.leaf[0])
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()

			elif(current_node.type == 'separator'):
				
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.c.push(current_node.leaf[0])

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)

				self.print_state()

			elif(current_node.type == 'assign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()

			elif(current_node.type == 'booleanexpression_binop'):
				print current_node.leaf[0]
				self.c.push(str(current_node.leaf))
				child0 = self.load_program(current_node.children[0]) 	
				child1 = self.load_program(current_node.children[1]) 	
				if(child0 is not None):			
					self.c.push(child0)
				if(child1 is not None):			
					self.c.push(child1)
				self.print_state()
			
			elif(current_node.type == 'boolassign'):

				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				child1 = self.load_program(current_node.children[1])
				if(child1 is not None):			
					self.c.push(child1)
				
				self.print_state()
			elif(current_node.type == 'booleanvalues'):
				self.c.push(current_node.leaf)
				
				self.print_state()
			
			elif(current_node.type == 'booleanexpressionnot'):
				child0 = self.load_program(current_node.children[0]) 					
				if(child0 is not None):			
					self.c.push(child0)
				self.c.push(str(current_node.leaf))
				
				self.print_state()
		else:
			return current_node		


	def run_program(self):

		
		while(not self.c.isEmpty() ):

			control = self.c.pop()

			if(isinstance(control,numbers.Number)):
				self.s.push(control)
				self.print_state();

			elif(isinstance(control,str)):
				if(control == 'add' or control == 'div' or control == 'mul' or control == 'sub'):
					self.evaluate_binary_op(control)	
				
				elif(control == 'and' or control == 'or'):
					self.evaluate_binary_bool(control)	
				
				elif(control=='tt' or control=='ff'):
					value = self.s.push(control);
					
					self.print_state();
				
				elif(control == ':='):
					variable = self.c.pop();
					value = self.s.pop();
					memoryPos = self.var_exists(variable)

					if(memoryPos == -1):
						memoryCell = MemoryCell(value,variable)
						self.m.append(memoryCell)
						#memAddress = MemoryAddress(len(self.m) - 1)
						#self.s.push(memAddress)
						#self.s.push(variable)
					else:
						self.m[memoryPos].data = value
						##memAddress = MemoryAddress(memoryPos)
						#self.s.push(memAddress)
				elif(control == ';'):
					self.print_state()
				else:
					self.s.push(control)
					self.print_state();


		
		self.print_state();


	def var_exists(self,varName):
		index = 0
		for cell in self.m:
			if(varName == cell.variableName):
				return index
			index+=1

		return -1

	def evaluate_binary_op(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()		
		if(isinstance(op1,str)):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();		
		if(isinstance(op2,str)):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();
		
		self.print_state()
		if(operator == 'add'):
			self.s.push(op1+op2);
		if(operator == 'sub'):
			self.s.push(op1-op2);
		if(operator == 'mul'):
			self.s.push(op1*op2);
		if(operator == 'div'):
			self.s.push(op1/op2);
		self.print_state();

	def evaluate_binary_bool(self,operator):
		op1 = self.s.pop()		
		op2 = self.s.pop()		
		if(isinstance(op1,str) and not (op1 == 'tt' or op1 == 'ff')):
			self.loadFromMemoryToS(op1)
			op1 = self.s.pop();		
		if(isinstance(op2,str)and not (op2 == 'tt' or op2 == 'ff')):
			self.loadFromMemoryToS(op2)
			op2 = self.s.pop();
		temp1 = op1 == 'tt'
		temp2 = op2 == 'tt'
		self.print_state()
		if(operator == 'and'):
			if(temp1 and temp2):
				self.s.push('tt')
			else:
				self.s.push('ff')
		if(operator == 'or'):
			if(temp1 or temp2):
				self.s.push('tt')
			else:
				self.s.push('ff')
		
		self.print_state();

	def loadFromMemoryToS(self,varName):
		address = self.var_exists(varName)
		memCell = self.m[address]
		self.s.push(memCell.data)
		self.print_state()






class Stack:
     def __init__(self):
         self.items = []

     def isEmpty(self):
         return self.items == []

     def push(self, item):
         self.items.append(item)

     def pop(self):
         return self.items.pop()

     def peek(self):
         return self.items[len(self.items)-1]

     def size(self):
         return len(self.items)

class MemoryCell:
	def __init__(self,data, var):
		self.data = data
		self.variableName = var
	def __repr__(self):
		return "M["+str(self.data)+"/"+str(self.variableName)+"]"


class MemoryAddress:
	def __init__(self, address):
		self.address = address	

	def __repr__(self):
		return "M("+str(self.address)+")"


