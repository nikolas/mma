{-# OPTIONS -fglasgow-exts #-}

{- metavar's dsl. do it this way until i know what can be removed
 -}

module MetaGL (
	GLC(..),
	rotate,
	scale,
	translate,
	identity,
	triangles,
	quads,
	serial,
	parallel,
	vertex,
	color,
	render
) where

import qualified Graphics.UI.GLUT as GL

import State

class GLCommand a where
	render :: a -> IO ()

data GLC = forall a. GLCommand a => GLC a
instance GLCommand GLC where
	render (GLC a) = render a

rotate :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
rotate a v1 v2 v3 = GLC $ ActionRotate a (GL.Vector3 v1 v2 v3)
scale :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
scale x y z = GLC $ ActionScale x y z
translate :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
translate x y z = GLC $ ActionTranslate (GL.Vector3 x y z)
identity = GLC $ (ActionIdentity :: MatrixActions GL.GLdouble)

data GL.MatrixComponent a => MatrixActions a = ActionRotate a (GL.Vector3 a)
	| ActionScale a a a
	| ActionTranslate (GL.Vector3 a)
	| ActionIdentity

instance GL.MatrixComponent a => GLCommand (MatrixActions a) where
	render = matrixActions
matrixActions (ActionRotate angle vec) = GL.rotate angle vec
matrixActions (ActionScale x y z) = GL.scale x y z
matrixActions (ActionTranslate vec) = GL.translate vec
matrixActions (ActionIdentity) = GL.loadIdentity

data GL.VertexComponent a => VertexActions a = ActionVertex (GL.Vertex3 a)
instance GL.VertexComponent a => GLCommand (VertexActions a) where
	render = vertexActions
vertexActions (ActionVertex vec) = GL.vertex vec
vertex :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
vertex x y z = GLC $ ActionVertex (GL.Vertex3 x y z)

data GL.ColorComponent a => ColorActions a = ActionColor (GL.Color3 a)
instance GL.ColorComponent a => GLCommand (ColorActions a) where
	render = colorActions
colorActions (ActionColor c) = GL.color c
color :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
color r g b = GLC $ ActionColor (GL.Color3 r g b)

triangles commands = GLC $ RenderTriangles commands
quads commands = GLC $ RenderQuads commands
serial commands = GLC $ RenderSerial commands
parallel commands = GLC $ RenderParallel commands

data RenderActions = RenderTriangles [GLC]
	| RenderQuads [GLC]
	| RenderSerial [GLC]
	| RenderParallel [GLC]
instance GLCommand RenderActions where
	render = renderActions
renderActions (RenderTriangles commands) = GL.renderPrimitive GL.Triangles $ mapM_ render commands
renderActions (RenderQuads commands) = GL.renderPrimitive GL.Quads $ mapM_ render commands
renderActions (RenderSerial commands) = mapM_ render commands
renderActions (RenderParallel commands) = mapM_ (GL.preservingMatrix . render) commands
