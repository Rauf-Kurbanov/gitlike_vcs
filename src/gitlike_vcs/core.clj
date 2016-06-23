(ns gitlike-vcs.core)

; TODO remove from project
;(require '[clojure.core.match :refer [match]])
(require '[clojure.set :refer [intersection]])

(def empty-commit
  { :snapshot nil
    :parent nil }
  )

(def empty-branch
  { :commit-id nil }
  )

(def empty-repo
  {:head     "master"
   :branches {"master" nil}
   :commits []
   }
  )

(defn commit
  [repo snapshot]
  (let [ {bs :branches cs :commits} repo
        curr-branch    (bs :head)
        new-commit     { :snapshot snapshot
                         :parent  (curr-branch :commit)}
        new-id         (count cs)
        updated-branch (assoc curr-branch :commit-id new-id)
        ]
    (assoc repo :head updated-branch
                :commits (conj cs new-commit))
    )
  )

; [repo hash]
(defn checkout [repo branch-name]
  (assoc repo :head branch-name)
  )

(defn branch [repo branch-name]
  (let [{h :head bs :branches} repo
        new-branch (bs h)]
    (assoc repo :branches (assoc bs branch-name new-branch))
    )
  )

;(defn merge [repo name])
;

(declare branch-history)
(declare last-common)

(defn rebase [repo branch-name]
  (let [ {h :head bs :branches cs :commits} repo
        bh-where (branch-history repo branch-name)
        bh-curr (branch-history repo h)
        lc (last-common bh-where bh-curr)
        last-comm-repo { :head branch-name
                        :branches (filter #(<= (% :commit-id) lc) bs)
                        :commits (filter #(<= % lc) cs)
                        }
        where-comms (filter #(> % lc) bh-where)
        curr-comms (filter #(> % lc) bh-curr)
        all-comms (flatten [where-comms, curr-comms])
        ]

        (loop [repo-version last-comm-repo, rest-comms all-comms]
          (if (empty? rest-comms)
            repo-version
            (recur
              (commit last-comm-repo ((rest-comms :head) :snapshot))
              (rest rest-comms)
                   )
            )

          )
    )
  )

(defn last-common
  [ids1 ids2]
  (let [ss1 (into (sorted-set) ids1)
        ss2 (into (sorted-set) ids2)]
    (last (intersection ss1 ss2))
    )
  )

(declare prev-ids)

(defn branch-history
  [repo branch-name]
  (let [ { bs :branches } repo
        last-comm-id (bs branch-name)
        ]
    (prev-ids last-comm-id repo)
    )
  )

(defn history [repo path]
  (let [ {h :head bs :branches} repo
        last-comm-id (get-in bs [h :commit-id])
        commit-ids (prev-ids last-comm-id repo)
        ]
    (map (fn [id] get-in repo (flatten [:commits id :snapshot path])
           )
         commit-ids
         )
    )
  )

; TODO unfold
(defn prev-ids
  [commit-id repo]
  (loop [curr-id commit-id, id-list [commit-id]]
    (let [parent-id (get-in repo [:commits curr-id :parent])]
      (if (nil? parent-id)
        id-list
        (recur
          parent-id
          (conj id-list parent-id)
          )
        )
      )
    )
  )

