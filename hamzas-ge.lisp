(in-package #:hamzas-ge)

(defvar *buf-stream* nil)
(defvar *gpu-arr* nil)

;;------------------------------------------------------------

(defclass camera ()
  ((pos :initform (v! 0 0 0)   :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *camera* (make-instance 'camera))

(defun get-world->view (camera)
  (m4:* (m4:translation (v3:negate (pos camera)))
        (q:to-mat4 (q:inverse (rot camera)))))

;;------------------------------------------------------------

(defclass thing ()
  ((pos :initform (v! 0 0 0)   :accessor pos)
   (rot :initform (q:identity) :accessor rot)))

(defvar *things* (loop for i below 40 collect
                   (make-instance 'thing)))

(defun get-model->world (thing)
  (m4:* (m4:translation (pos thing))
        (q:to-mat4 (rot thing))))

(defun update-thing (thing)
  (with-slots (pos) thing
    (setf (y pos)
          (mod (- (y pos) 0.1) 40f0))))

;;------------------------------------------------------------

(defun-g main-vert-stage ((vert g-pnt)
                          &uniform (now :float)
                                   (model->world :mat4)
                                   (world->view :mat4)
                                   (view->clip :mat4))
  (let* ((inst-id gl-instance-id)
         (now (+ now inst-id))

         (vert-pos (pos vert))

         ;; Model -> World Space
         ;; (vert-pos (+ vert-pos (v! (* 2 (sin now))
         ;;                           (* 2 (cos now))
         ;;                           (+ -2 (sin (* 5 now))))))
         ;; (vert-pos (+ vert-pos (v! 0 0 -5)))
         (vert-pos (v! vert-pos 1))
         (vert-pos (* model->world vert-pos))

         ;; World -> View Space
         ;; (vert-pos (- vert-pos cam-pos))
         ;; (vert-pos (* cam-rot vert-pos)))
         (vert-pos (* world->view vert-pos)))

  (values
    ;; View -> Clip Space
    (* view->clip vert-pos))))

(defun-g main-frag-stage ()
  (let* ((object-color (v! 0 1 0 0))
         (ambient 0.1)
         (light-amount ambient))
    (* object-color light-amount)))

(defpipeline-g main-pipeline ()
  (main-vert-stage g-pnt)
  (main-frag-stage))

;;------------------------------------------------------------

(defun now ()
  (/ (float (get-internal-real-time))
     5000000))

(defun draw ()
  (step-host) ;; Inform us about Input Events

  ;; Set viewport resolution to window resolution
  (setf (resolution (current-viewport))
        (surface-resolution (current-surface)))

  ;; (setf (pos *camera*) (q:from-axis-angle
  ;;                   (v! 1 0 0)
  ;;                   (radians (* 10 (sin (* 15 (now)))))))

  ;; (setf (rot *camera*) (q:from-axis-angle
  ;;                        (v! 1 0 0)
  ;;                        (radians (* 50 (cos (* 30 (now)))))))

  (clear)

  (loop :for thing :in *things* :do
    (update-thing thing)
    (map-g #'main-pipeline *buf-stream* ; Map over some pipline
           :now (now)
           :model->world (get-model->world thing)
           :world->view (get-world->view *camera*)
           :view->clip (rtg-math.projection:perspective
                          (x (resolution (current-viewport)))
        			      (y (resolution (current-viewport)))
            	          0.1	
                	      30f0
                          70f0)))
  (swap))

(defun init ()
  (unless *buf-stream*
    (destructuring-bind (vert index)
      (nineveh.mesh.data.primitives:cube-gpu-arrays)
    (setf *buf-stream* (make-buffer-stream vert :index-array index))))

  (loop :for thing :in *things* :do
				   (setf (pos thing)
					     (v3:+ (v! 0 0 -25)
						       (v! (- (random 25) 15)
								   (- (random 35) 10)
								   (- (random 20) 10)))))

  (setf (pos *camera*) (v! -2.5 15 0)))

(def-simple-main-loop play (:on-start #'init)
  (draw))































