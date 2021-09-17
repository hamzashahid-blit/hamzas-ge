(in-package #:hamzas-ge)

(defvar *buf-stream* nil)
(defvar *vert-gpu-arr* nil)
(defvar *cam-pos* (v! 0 0 0))
(defvar *cam-rot* (q:identity))

(defun-g some-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                          ;; (cam-pos :vec3)
                          ;; (cam-rot :mat3)
                          (world->view :mat4)
                          (perspective :mat4))
  (let* ((now (+ now gl-instance-id))
         (pos (pos vert))
         (color (+ pos (v! 0.5 0.5 0.5)))

         ;; position the vertex 
         ;; model-space to world-space
         (pos (+ pos (v! (* 2 (sin now))
                         (* 2 (cos now))
                         (+ -6 (sin (* 4 now))))))
         (pos (v! pos 1))

         ;; camera stuff
         ;; world-space to view-space
         ;; (pos (- pos cam-pos))
         ;; (pos (* pos cam-rot)))
         (pos (* pos world->view)))
  (values
    ;; view-space to clip-space
    (* perspective pos)
    (:smooth color))))

(defun-g some-frag-stage ((color :vec3))
  color)

(defpipeline-g some-pipeline ()
  (some-vert-stage g-pnt)
  (some-frag-stage :vec3))

(defun now ()
  (/ (float (get-internal-real-time))
  4200000))

(defun get-world->view-space()
  (m4:* (m4:translation (v3:negate *cam-pos*))
        (q:to-mat4 (q:inverse *cam-rot*))))

(defun draw ()
  (step-host)

  ;; Set the viewport res to the window/surface res
  (setf (resolution (current-viewport))
    (surface-resolution (current-surface)))

  ;; (setf *cam-rot* (q:from-axis-angle 
  ;;                   (v! 0 1 0)
  ;;   				(radians (* 20 (sin (* 30 (now)))))))

  (clear)
  (with-instances 10
    (map-g #'some-pipeline *buf-stream*
      :now (now)
      ;; :cam-pos *cam-pos*
      ;; :cam-rot (q:to-mat3 (q:inverse *cam-rot*))
      :world->view (get-world->view-space)
      :perspective (rtg-math.projection:perspective
				     (x (resolution (current-viewport)))
				     (y (resolution (current-viewport)))
				     0.1 
				     30f0 
                     60f0)))
  (swap))

(defun init ()
  (unless *buf-stream*
    (destructuring-bind (vert index)
      (nineveh.mesh.data.primitives:sphere-gpu-arrays)
      (setf *buf-stream*
        (make-buffer-stream vert :index-array index))))

  (unless *vert-gpu-arr*
    (setf *vert-gpu-arr*
      (first (buffer-stream-gpu-arrays *buf-stream*)))))

(def-simple-main-loop play (:on-start #'init)
  (draw))
