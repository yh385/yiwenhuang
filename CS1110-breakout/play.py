# play.py
# Yiwen Huang (yh385) and Vicky Chou (vc265)
# December 11, 2015
# We used Professor White's code for update(self,dt) from arrows.py for our
# helper method updatePaddle(). Minor changes were made.
"""Subcontroller module for Breakout

This module contains the subcontroller to manage a single game in the Breakout
App. Instances of Play represent a single game. If you want to restart a new
game, you are expected to make a new instance of Play.

The subcontroller Play manages the paddle, ball, and bricks. These are model
objects. Their classes are defined in models.py.

Most of your work on this assignment will be in either this module or models.py.
Whether a helper method belongs in this module or models.py is often a complicated
issue. If you do not know, ask on Piazza and we will answer.
"""
from constants import *
from game2d import *
from models import *


# PRIMARY RULE: Play can only access attributes in models.py via getters/setters
# Play is NOT allowed to access anything in breakout.py (Subcontrollers are not
# permitted to access anything in their parent. To see why, take CS 3152)

class Play(object):
    """An instance controls a single game of breakout.
    
    This subcontroller has a reference to the ball, paddle, and bricks. It
    animates the ball, removing any bricks as necessary. When the game is won,
    it stops animating. You should create a NEW instance of Play (in Breakout)
    if you want to make a new game.
    
    If you want to pause the game, tell this controller to draw, but do not
    update. See subcontrollers.py from Lecture 25 for an example.
    
    INSTANCE ATTRIBUTES:
        _paddle [Paddle]: the paddle to play with 
        _bricks [list of Brick]: the list of bricks still remaining 
        _ball   [Ball, or None if waiting for a serve]: the ball to animate
        _tries  [int >= 0]: the number of tries left 
    
    As you can see, all of these attributes are hidden. You may find that you
    want to access an attribute in class Breakout. It is okay if you do, but you
    MAY NOT ACCESS THE ATTRIBUTES DIRECTLY. You must use a getter and/or setter
    for any attribute that you need to access in Breakout. Only add the getters
    and setters that you need for Breakout.
    
    You may change any of the attributes above as you see fit. For example, you
    may want to add new objects on the screen (e.g power-ups). If you make
    changes, please list the changes with the invariants.
                  
    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
    """
    
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getBall(self):
        """gets the _ball from this class"""
        if self._ball==None:
            return None
        else:
            return self._ball
    
    def setBall(self):
        """sets the _ball to an instance of the class Ball"""
        self._ball=Ball()
        
    def getTries(self):
        """Returns the number of tries left"""
        return self._tries
    
    def getBricks(self):
        """Returns the amount of bricks left"""
        return (len(self._bricks))
    
    # INITIALIZER (standard form) TO CREATE PADDLES AND BRICKS
    def __init__(self):
        """Initializer: Creates a paddle and the bricks for the game Breakout,
        and sets the number of turns and the score to 0.
        """
        self._bricks = []
        for a in range(BRICKS_IN_ROW):
            for b in range(BRICK_ROWS):
                if BRICK_ROWS > 10:
                    c = b%10
                    self._bricks = self._bricks
                    + [Brick((BRICK_SEP_H/2+BRICK_WIDTH/2)+((BRICK_SEP_H+BRICK_WIDTH)*a),
                        GAME_HEIGHT-((BRICK_Y_OFFSET+BRICK_HEIGHT/2)+
                            ((BRICK_SEP_V+BRICK_HEIGHT)*b)), BRICK_COLORS[c])]
                else:
                    self._bricks = self._bricks + [Brick((BRICK_SEP_H/2+BRICK_WIDTH/2)
                        +((BRICK_SEP_H+BRICK_WIDTH)*a), GAME_HEIGHT-
                        ((BRICK_Y_OFFSET+BRICK_HEIGHT/2)+((BRICK_SEP_V+BRICK_HEIGHT)*b)),
                        BRICK_COLORS[b])]
        self._paddle=Paddle(PAD_MIDPOINT)
        self._ball=None
        self._tries=NUMBER_TURNS
      
    # UPDATE METHODS TO MOVE PADDLE, SERVE AND MOVE THE BALL
    def updatePaddle(self,inp):
        """Updates the paddle in the game Breakout.
        
        If the left arrow key is pressed, the paddle will move to the left.
        If the right arrow key is pressed, the paddle will move to the right.
        The paddle should not go off the screen.
        
        Parameter inp: the user input
        Precondition: the user input is an instance of GInput
        """
        # This code was taken from Professor White's method update(self,dt)
        # from arrows.py. We changed da to dx.
        assert isinstance(inp, GInput)
        dx=0
        if inp.is_key_down('left'):
            dx-=ANIMATION_STEP
            
        if inp.is_key_down('right'):
            dx+=ANIMATION_STEP
        
        self._paddle.x += dx
        self._paddle.x = max(PADDLE_WIDTH/2, self._paddle.x)
        self._paddle.x = min (GAME_WIDTH-PADDLE_WIDTH/2, self._paddle.x)
               
    # DRAW METHOD TO DRAW THE PADDLES, BALL, AND BRICKS
    def draw(self,view):
        """Draws the bricks, the paddle, and the ball for the game Breakout.
        
        Parameter view: the window Breakout is played in
        Precondition: the window must be an instance of GView
        """
        assert isinstance(view,GView)
        for d in self._bricks:
            d.draw(view)
        self._paddle.draw(view)
        if self._ball != None:
            self._ball.draw(view)

    def updateBall(self):
        """Returns None if the ball hits a brick. Updates the ball in the game Breakout.
        
        Each time this method is called, the ball moves one step. If the ball
        hits the bottom of the window, then the number of tries decreases by one
        and the ball disappears.
        """
        vx = self._ball.getVX()
        vy = self._ball.getVY()
        self._ball.move()
        
        if self._ball.x+BALL_RADIUS>GAME_WIDTH:
            self._ball.setVX(-vx)
        
        if self._ball.x-BALL_RADIUS<=0:
            self._ball.setVX(-vx)
        
        if self._ball.y+BALL_RADIUS>GAME_HEIGHT:
            self._ball.setVY(-vy)
        
        if self._paddle.collides(self._ball)==True and self._ball._vy<0:
            self._ball.setVY(-vy)

        for brick in self._bricks:
            if brick.collides(self._ball)==True:
                self._ball.setVY(-vy)
                self._bricks.remove(brick)
                return None
            
        if self._ball.y-BALL_RADIUS<=0:
            self._tries-=1
            self._ball=None
   
    # HELPER METHODS FOR PHYSICS AND COLLISION DETECTION
    
# ADD ANY ADDITIONAL METHODS (FULLY SPECIFIED) HERE
