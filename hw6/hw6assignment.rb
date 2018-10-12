# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_Pieces 	<< rotations([[0, 0], [1, 0], [0, 1], [1, 1],[2,1]]) 
  All_Pieces 	<< [[[0, 0], [-1, 0], [1, 0], [-2, 0], [2, 0]],
  					[[0, 0], [0, -1], [0, 1], [0, -2], [0, 2]]]
  All_Pieces 	<< rotations([[0, 0], [0, 1], [1,1]])
  All_My_Pieces = All_Pieces
  Cheat_Piece = [[[[0,0]]]]	
  # your enhancements here
  @@cheat_times = 0
  @@is_cheat_falling = false

  def self.cheat_times
  	@@cheat_times
  end 

  def cheat_times
  	@@cheat_times
  end

  def self.is_cheat_falling
  	@@is_cheat_falling
  end 

  def is_cheat_falling
  	@@is_cheat_falling
  end	

  def self.next_piece (board) 	
  	if @@cheat_times > 0
  		@@cheat_times -= 1
  		@@is_cheat_falling = true
  		MyPiece.new(Cheat_Piece.sample, board)
  	else
  		@@is_cheat_falling = false
  		MyPiece.new(All_My_Pieces.sample, board)
  	end
  end	    
end

class MyBoard < Board
  # your enhancements here
  def rotate_180_degrees
  	if !game_over? and @game.is_running?
  		@current_block.move(0, 0, 2)
  	end
  	draw
  end

  def cheat
  	if !game_over? and @score >= 100
  		if not ((MyPiece.cheat_times != 0) and (MyPiece.is_cheat_falling == false))
  			@score -= 100
  			MyPiece.class_variable_set(:@@cheat_times, MyPiece.cheat_times + 1)
  			@game.update_score
  		end
  	end
  end

  def store_current
  	locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
  	@canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  def key_bindings
  	super
  	@root.bind('u', lambda {@board.rotate_180_degrees})
  	@root.bind('c', lambda {@board.cheat})
  end
end