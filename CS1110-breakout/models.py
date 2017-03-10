# models.py
# Yiwen Huang (yh385) and Vicky Chou (vc265)
# December 11, 2015
"""Models module for Breakout

This module contains the model classes for the Breakout game. That is anything
that you interact with on the screen is model: the paddle, the ball, and any of
the bricks.

Technically, just because something is a model does not mean there has to be a
special class for it. Unless you need something special, both paddle and
individual bricks could just be instances of GRectangle. However, we do need
something special: collision detection. That is why we have custom classes.

You are free to add new models to this module. You may wish to do this when you
add new features to your game. If you are unsure about whether to make a new
class or not, please ask on Piazza.
"""
import random # To randomly generate the ball velocity
from constants import *
from game2d import *


# PRIMARY RULE: Models are not allowed to access anything except the module
# constants.py.
# If you need extra information from Play, then it should be a parameter in your
# method, and Play should pass it as a argument when it calls the method.

class Paddle(GRectangle):
    """An instance is the game paddle.
    
    This class contains a method to detect collision with the ball, as well as
    move it left and right. You may wish to add more features to this class.
    
    The attributes of this class are those inherited from GRectangle.
    
    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
    _x       [int or float]
            the midpoint of the paddle
    _collide [boolean]
            True if ball has collided with paddle, False otherwise
    """
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    
    # INITIALIZER TO CREATE A NEW PADDLE
    def __init__(self, x1):
        """Initializer: Creates a new paddle for the game Breakout.
        
        Parameter x1: the width of the paddle
        Precondition: the width must be an float or int < GAME_WDITH
        """
        assert ((type(x1) == float) or type(x1)==int ) and (x1 < GAME_WIDTH)
        GRectangle.__init__(self,x=x1, y=PADDLE_OFFSET, width=PADDLE_WIDTH,
                            height=PADDLE_HEIGHT, fillcolor=colormodel.BLACK,
                            linecolor=colormodel.BLACK)
        self._x=x1
      
    # METHODS TO MOVE THE PADDLE AND CHECK FOR COLLISIONS
    def collides(self,ball):
        """Returns: True if the ball collides with the paddle.
        
        Parameter ball: the ball to check
        Precondition: the ball is of class Ball
        """
        assert isinstance(ball, Ball)
        corners = [[ball.x-BALL_RADIUS,ball.y+BALL_RADIUS],
                  [ball.x+BALL_RADIUS,ball.y+BALL_RADIUS],
                  [ball.x-BALL_RADIUS, ball.y-BALL_RADIUS],
                  [ball.x+BALL_RADIUS,ball.y-BALL_RADIUS]]
        self._collide=False
        for point in corners:
            if self.contains(point[0],point[1]):
                self._collide=True
        return self._collide
    
    # ADD MORE METHODS (PROPERLY SPECIFIED) AS NECESSARY


class Brick(GRectangle):
    """An instance is the game paddle.
    
    This class contains a method to detect collision with the ball. You may wish
    to add more features to this class.
    
    The attributes of this class are those inherited from GRectangle.
    
    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
    
    """
   
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    
    # INITIALIZER TO LAYOUT BRICKS ON THE SCREEN
    def __init__(self, x1, y1, fillcolor1):
        """Initializer: Creates the bricks for the game Breakout.
        
        Parameter x1: the width of every brick
        Precondition: the width must be an int or float < GAME_WIDTH
        
        Parameter y1: the height of every brick
        Precondition: the height must be an int or float< GAME_HEIGHT
        
        Parameter fillcolor1: the color of the bricks
        Precondition: the color must be an instance of colormodel
        """
        assert ((type(x1) == int) or (type(x1)==float )) and (x1 < GAME_WIDTH)
        assert ((type(y1) == int) or (type(xy1)==float )) and (y1 < GAME_HEIGHT)
        assert isinstance(fillcolor1, colormodel.RGB)
        GRectangle.__init__(self, x=x1, y=y1, width=BRICK_WIDTH, height=
                            BRICK_HEIGHT, fillcolor=fillcolor1,
                            linecolor=fillcolor1)
           
    # ADD MORE METHODS (PROPERLY SPECIFIED) AS NECESSARY
    def collides(self,ball):
        """Returns: True if the ball collides with a brick, False otherwise.
        
        Parameter ball: the ball to check
        Precondition: the ball is of class Ball
        """
        assert isinstance(ball, Ball)
        corners=[[ball.x-BALL_RADIUS,ball.y+BALL_RADIUS],
                  [ball.x+BALL_RADIUS,ball.y+BALL_RADIUS],
                  [ball.x-BALL_RADIUS, ball.y-BALL_RADIUS],
                  [ball.x+BALL_RADIUS,ball.y-BALL_RADIUS]]
        collide=False
        for point in corners:
            if self.contains(point[0],point[1]):
                collide=True
        return collide
    
    
class Ball(GEllipse):
    """Instance is a game ball.
    
    We extend GEllipse because a ball must have additional attributes for
    velocity. This class adds this attributes and manages them.
    
    INSTANCE ATTRIBUTES:
        _vx [int or float]: Velocity in x direction 
        _vy [int or float]: Velocity in y direction 
    
    The class Play will need to look at these attributes, so you will need
    getters for them.  However, it is possible to write this assignment with no
    setters for the velocities.
    
    How? The only time the ball can change velocities is if it hits an obstacle
    (paddle or brick) or if it hits a wall.  Why not just write methods for these
    instead of using setters?  This cuts down on the amount of code in Gameplay.
    
    NOTE: The ball does not have to be a GEllipse. It could be an instance
    of GImage (why?). This change is allowed, but you must modify the class
    header up above.
    
    LIST MORE ATTRIBUTES (AND THEIR INVARIANTS) HERE IF NECESSARY
    """
    
    # GETTERS AND SETTERS (ONLY ADD IF YOU NEED THEM)
    def getVY(self):
        """Returns: the y component of velocity of the ball."""
        return self._vy
    
    def getVX(self):
        """Returns: the x component of velocity of the ball."""
        return self._vx
    
    def setVY(self, vy):
        """Sets the y component of velocity of the ball."""
        self._vy = vy
      
    def setVX(self, vx):
        """Sets the x component of velocity of the ball."""
        self._vx = vx
  
    def __init__(self):
        """Initializer: Sets the velocity and the dimensions of the ball."""
        GEllipse.__init__(self)
        self._vx = random.uniform(1.0,5.0)
        self._vx = self._vx*random.choice([-1,1])
        self._vy = -BALL_SPEED
        self.x = (GAME_WIDTH-BALL_DIAMETER)/2
        self.y = PADDLE_OFFSET+PADDLE_HEIGHT+GAME_HEIGHT/2
        self.width = BALL_DIAMETER
        self.height = BALL_DIAMETER
        self.fillcolor = colormodel.BLUE
    
    # METHODS TO MOVE AND/OR BOUNCE THE BALL
    def move(self):
        """Moves the ball one step."""
        self.x+=self._vx
        self.y+=self._vy
    
    
    # ADD MORE METHODS (PROPERLY SPECIFIED) AS NECESSARY
    
    

# IF YOU NEED ADDITIONAL MODEL CLASSES, THEY GO HERE