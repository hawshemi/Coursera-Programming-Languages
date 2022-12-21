# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.


class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces +
              # addition pieces
              [
                [
                  [[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # extra long (two cases)
                  [[0, 0], [0, -2], [0, -1], [0, 1], [0, 2]]
                ],
                rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]),
                # [[[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]]],
                rotations([[0, 0], [1, 0], [0, 1]])
              ]

  Cheat_Piece = [[[0, 0]]]

  
  # your enhancements here

  def initialize (point_array, board)
    super(point_array, board)
    @point_count = (@all_rotations[@rotation_index]).size
  end

  def point_count
    @point_count
  end

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    MyPiece.new(Cheat_Piece, board)
  end

end


class MyBoard < Board

  def initialize(game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  # your enhancements here
  def rotate_180
    rotate_counter_clockwise
    rotate_counter_clockwise
  end

  # gets the next piece
  def next_piece
    if (@cheat)
      @cheat = false
      @score -= 100
      @current_block = MyPiece.cheat_piece(self)
    else
      @current_block = MyPiece.next_piece(self)
    end

    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    point_count = @current_block.point_count
    displacement = @current_block.position
    (0..(point_count-1)).each{ |index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def cheat_next
    if @score >= 100
      @cheat = true
    end
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
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat_next})
  end
end
