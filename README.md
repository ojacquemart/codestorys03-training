CodeStory S03
=====

http://www.code-story.net/blog/posts/s03e01.1


Events (just respond HTTP 200 return code)

* /reset?lowerFloor=0&higherFloor=19&cause=information+message
* /call?atFloor=[0-19]&to=[UP|DOWN]
* /go?floorToGo=[0-19]
* /userHasEntered
* /userHasExited

response

* /nextCommand : body of the request must contain NOTHING, UP, DOWN, OPEN or CLOSE