(asdf:defsystem "boxer-sunrise-opengl"
  :version "3.4.23"
  :author "Steven Githens"
  :license "BSD-3-Clause"
  :depends-on (:boxer-sunrise-core
               :3d-matrices
               :cl-opengl
               :for)
  :components ((:file "package")
               (:file "pixmap")
               (:file "stencils")
               (:file "graphics-canvas")
               ;;  (:file "simple-line-shapes")
               (:file "line-shapes")
               (:file "shader-shapes")
               (:file "box-models-meshes")
               (:file "draw-low-opengl330")
               #+(not delivering)
               (:file "freetype-fonts")
               (:file "draw-low-opengl")
               (:file "opengl-draw-bridge")
               (:file "perspective"))
  :description "OpenGL Implementation of Boxer Rendering")
