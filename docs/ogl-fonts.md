2021-05-21 This font structure is now deprecated and removed, but for the purposes of debugging
old saved boxer files, we'll keep this reference of the structure of the fixed fonts around for a bit.

(loop for font-family across boxer::*font-cache* do
                  (when (not (null font-family))
                    (loop for font-sizes across font-family do
                          (loop for font-style across font-sizes do
                                (print font-style)))))

#<OGLFont Arial 8 [NIL]>
#<OGLFont Arial 8  BOLD[NIL]>
#<OGLFont Arial 8  ITALIC[NIL]>
#<OGLFont Arial 8  BOLD ITALIC[NIL]>
#<OGLFont Arial 9 [NIL]>
#<OGLFont Arial 9  BOLD[NIL]>
#<OGLFont Arial 9  ITALIC[NIL]>
#<OGLFont Arial 9  BOLD ITALIC[NIL]>
#<OGLFont Arial 10 [NIL]>
#<OGLFont Arial 10  BOLD[NIL]>
#<OGLFont Arial 10  ITALIC[NIL]>
#<OGLFont Arial 10  BOLD ITALIC[NIL]>
#<OGLFont Arial 12 [NIL]>
#<OGLFont Arial 12  BOLD[NIL]>
#<OGLFont Arial 12  ITALIC[NIL]>
#<OGLFont Arial 12  BOLD ITALIC[NIL]>
#<OGLFont Arial 14 [NIL]>
#<OGLFont Arial 14  BOLD[NIL]>
#<OGLFont Arial 14  ITALIC[NIL]>
#<OGLFont Arial 14  BOLD ITALIC[NIL]>
#<OGLFont Arial 16 [NIL]>
#<OGLFont Arial 16  BOLD[NIL]>
#<OGLFont Arial 16  ITALIC[NIL]>
#<OGLFont Arial 16  BOLD ITALIC[NIL]>
#<OGLFont Arial 20 [NIL]>
#<OGLFont Arial 20  BOLD[NIL]>
#<OGLFont Arial 20  ITALIC[NIL]>
#<OGLFont Arial 20  BOLD ITALIC[NIL]>
#<OGLFont Arial 24 [NIL]>
#<OGLFont Arial 24  BOLD[NIL]>
#<OGLFont Arial 24  ITALIC[NIL]>
#<OGLFont Arial 24  BOLD ITALIC[NIL]>
#<OGLFont Arial 28 [NIL]>
#<OGLFont Arial 28  BOLD[NIL]>
#<OGLFont Arial 28  ITALIC[NIL]>
#<OGLFont Arial 28  BOLD ITALIC[NIL]>
#<OGLFont Arial 32 [NIL]>
#<OGLFont Arial 32  BOLD[NIL]>
#<OGLFont Arial 32  ITALIC[NIL]>
#<OGLFont Arial 32  BOLD ITALIC[NIL]>
#<OGLFont Arial 40 [NIL]>
#<OGLFont Arial 40  BOLD[NIL]>
#<OGLFont Arial 40  ITALIC[NIL]>
#<OGLFont Arial 40  BOLD ITALIC[NIL]>
#<OGLFont Arial 48 [NIL]>
#<OGLFont Arial 48  BOLD[NIL]>
#<OGLFont Arial 48  ITALIC[NIL]>
#<OGLFont Arial 48  BOLD ITALIC[NIL]>
#<OGLFont Arial 56 [NIL]>
#<OGLFont Arial 56  BOLD[NIL]>
#<OGLFont Arial 56  ITALIC[NIL]>
#<OGLFont Arial 56  BOLD ITALIC[NIL]>
#<OGLFont Arial 64 [NIL]>
#<OGLFont Arial 64  BOLD[NIL]>
#<OGLFont Arial 64  ITALIC[NIL]>
#<OGLFont Arial 64  BOLD ITALIC[NIL]>
#<OGLFont Courier New 8 [NIL]>
#<OGLFont Courier New 8  BOLD[NIL]>
#<OGLFont Courier New 8  ITALIC[NIL]>
#<OGLFont Courier New 8  BOLD ITALIC[NIL]>
#<OGLFont Courier New 9 [NIL]>
#<OGLFont Courier New 9  BOLD[NIL]>
#<OGLFont Courier New 9  ITALIC[NIL]>
#<OGLFont Courier New 9  BOLD ITALIC[NIL]>
#<OGLFont Courier New 10 [NIL]>
#<OGLFont Courier New 10  BOLD[NIL]>
#<OGLFont Courier New 10  ITALIC[NIL]>
#<OGLFont Courier New 10  BOLD ITALIC[NIL]>
#<OGLFont Courier New 12 [NIL]>
#<OGLFont Courier New 12  BOLD[NIL]>
#<OGLFont Courier New 12  ITALIC[NIL]>
#<OGLFont Courier New 12  BOLD ITALIC[NIL]>
#<OGLFont Courier New 14 [NIL]>
#<OGLFont Courier New 14  BOLD[NIL]>
#<OGLFont Courier New 14  ITALIC[NIL]>
#<OGLFont Courier New 14  BOLD ITALIC[NIL]>
#<OGLFont Courier New 16 [NIL]>
#<OGLFont Courier New 16  BOLD[NIL]>
#<OGLFont Courier New 16  ITALIC[NIL]>
#<OGLFont Courier New 16  BOLD ITALIC[NIL]>
#<OGLFont Courier New 20 [NIL]>
#<OGLFont Courier New 20  BOLD[NIL]>
#<OGLFont Courier New 20  ITALIC[NIL]>
#<OGLFont Courier New 20  BOLD ITALIC[NIL]>
#<OGLFont Courier New 24 [NIL]>
#<OGLFont Courier New 24  BOLD[NIL]>
#<OGLFont Courier New 24  ITALIC[NIL]>
#<OGLFont Courier New 24  BOLD ITALIC[NIL]>
#<OGLFont Courier New 28 [NIL]>
#<OGLFont Courier New 28  BOLD[NIL]>
#<OGLFont Courier New 28  ITALIC[NIL]>
#<OGLFont Courier New 28  BOLD ITALIC[NIL]>
#<OGLFont Courier New 32 [NIL]>
#<OGLFont Courier New 32  BOLD[NIL]>
#<OGLFont Courier New 32  ITALIC[NIL]>
#<OGLFont Courier New 32  BOLD ITALIC[NIL]>
#<OGLFont Courier New 40 [NIL]>
#<OGLFont Courier New 40  BOLD[NIL]>
#<OGLFont Courier New 40  ITALIC[NIL]>
#<OGLFont Courier New 40  BOLD ITALIC[NIL]>
#<OGLFont Courier New 48 [NIL]>
#<OGLFont Courier New 48  BOLD[NIL]>
#<OGLFont Courier New 48  ITALIC[NIL]>
#<OGLFont Courier New 48  BOLD ITALIC[NIL]>
#<OGLFont Courier New 56 [NIL]>
#<OGLFont Courier New 56  BOLD[NIL]>
#<OGLFont Courier New 56  ITALIC[NIL]>
#<OGLFont Courier New 56  BOLD ITALIC[NIL]>
#<OGLFont Courier New 64 [NIL]>
#<OGLFont Courier New 64  BOLD[NIL]>
#<OGLFont Courier New 64  ITALIC[NIL]>
#<OGLFont Courier New 64  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 8 [NIL]>
#<OGLFont Times New Roman 8  BOLD[NIL]>
#<OGLFont Times New Roman 8  ITALIC[NIL]>
#<OGLFont Times New Roman 8  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 9 [NIL]>
#<OGLFont Times New Roman 9  BOLD[NIL]>
#<OGLFont Times New Roman 9  ITALIC[NIL]>
#<OGLFont Times New Roman 9  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 10 [NIL]>
#<OGLFont Times New Roman 10  BOLD[NIL]>
#<OGLFont Times New Roman 10  ITALIC[NIL]>
#<OGLFont Times New Roman 10  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 12 [NIL]>
#<OGLFont Times New Roman 12  BOLD[NIL]>
#<OGLFont Times New Roman 12  ITALIC[NIL]>
#<OGLFont Times New Roman 12  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 14 [NIL]>
#<OGLFont Times New Roman 14  BOLD[NIL]>
#<OGLFont Times New Roman 14  ITALIC[NIL]>
#<OGLFont Times New Roman 14  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 16 [NIL]>
#<OGLFont Times New Roman 16  BOLD[NIL]>
#<OGLFont Times New Roman 16  ITALIC[NIL]>
#<OGLFont Times New Roman 16  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 20 [NIL]>
#<OGLFont Times New Roman 20  BOLD[NIL]>
#<OGLFont Times New Roman 20  ITALIC[NIL]>
#<OGLFont Times New Roman 20  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 24 [NIL]>
#<OGLFont Times New Roman 24  BOLD[NIL]>
#<OGLFont Times New Roman 24  ITALIC[NIL]>
#<OGLFont Times New Roman 24  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 28 [NIL]>
#<OGLFont Times New Roman 28  BOLD[NIL]>
#<OGLFont Times New Roman 28  ITALIC[NIL]>
#<OGLFont Times New Roman 28  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 32 [NIL]>
#<OGLFont Times New Roman 32  BOLD[NIL]>
#<OGLFont Times New Roman 32  ITALIC[NIL]>
#<OGLFont Times New Roman 32  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 40 [NIL]>
#<OGLFont Times New Roman 40  BOLD[NIL]>
#<OGLFont Times New Roman 40  ITALIC[NIL]>
#<OGLFont Times New Roman 40  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 48 [NIL]>
#<OGLFont Times New Roman 48  BOLD[NIL]>
#<OGLFont Times New Roman 48  ITALIC[NIL]>
#<OGLFont Times New Roman 48  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 56 [NIL]>
#<OGLFont Times New Roman 56  BOLD[NIL]>
#<OGLFont Times New Roman 56  ITALIC[NIL]>
#<OGLFont Times New Roman 56  BOLD ITALIC[NIL]>
#<OGLFont Times New Roman 64 [NIL]>
#<OGLFont Times New Roman 64  BOLD[NIL]>
#<OGLFont Times New Roman 64  ITALIC[NIL]>
#<OGLFont Times New Roman 64  BOLD ITALIC[NIL]>
#<OGLFont Verdana 8 [NIL]>
#<OGLFont Verdana 8  BOLD[NIL]>
#<OGLFont Verdana 8  ITALIC[NIL]>
#<OGLFont Verdana 8  BOLD ITALIC[NIL]>
#<OGLFont Verdana 9 [NIL]>
#<OGLFont Verdana 9  BOLD[NIL]>
#<OGLFont Verdana 9  ITALIC[NIL]>
#<OGLFont Verdana 9  BOLD ITALIC[NIL]>
#<OGLFont Verdana 10 [NIL]>
#<OGLFont Verdana 10  BOLD[NIL]>
#<OGLFont Verdana 10  ITALIC[NIL]>
#<OGLFont Verdana 10  BOLD ITALIC[NIL]>
#<OGLFont Verdana 12 [NIL]>
#<OGLFont Verdana 12  BOLD[NIL]>
#<OGLFont Verdana 12  ITALIC[NIL]>
#<OGLFont Verdana 12  BOLD ITALIC[NIL]>
#<OGLFont Verdana 14 [NIL]>
#<OGLFont Verdana 14  BOLD[NIL]>
#<OGLFont Verdana 14  ITALIC[NIL]>
#<OGLFont Verdana 14  BOLD ITALIC[NIL]>
#<OGLFont Verdana 16 [NIL]>
#<OGLFont Verdana 16  BOLD[NIL]>
#<OGLFont Verdana 16  ITALIC[NIL]>
#<OGLFont Verdana 16  BOLD ITALIC[NIL]>
#<OGLFont Verdana 20 [NIL]>
#<OGLFont Verdana 20  BOLD[NIL]>
#<OGLFont Verdana 20  ITALIC[NIL]>
#<OGLFont Verdana 20  BOLD ITALIC[NIL]>
#<OGLFont Verdana 24 [NIL]>
#<OGLFont Verdana 24  BOLD[NIL]>
#<OGLFont Verdana 24  ITALIC[NIL]>
#<OGLFont Verdana 24  BOLD ITALIC[NIL]>
#<OGLFont Verdana 28 [NIL]>
#<OGLFont Verdana 28  BOLD[NIL]>
#<OGLFont Verdana 28  ITALIC[NIL]>
#<OGLFont Verdana 28  BOLD ITALIC[NIL]>
#<OGLFont Verdana 32 [NIL]>
#<OGLFont Verdana 32  BOLD[NIL]>
#<OGLFont Verdana 32  ITALIC[NIL]>
#<OGLFont Verdana 32  BOLD ITALIC[NIL]>
#<OGLFont Verdana 40 [NIL]>
#<OGLFont Verdana 40  BOLD[NIL]>
#<OGLFont Verdana 40  ITALIC[NIL]>
#<OGLFont Verdana 40  BOLD ITALIC[NIL]>
#<OGLFont Verdana 48 [NIL]>
#<OGLFont Verdana 48  BOLD[NIL]>
#<OGLFont Verdana 48  ITALIC[NIL]>
#<OGLFont Verdana 48  BOLD ITALIC[NIL]>
#<OGLFont Verdana 56 [NIL]>
#<OGLFont Verdana 56  BOLD[NIL]>
#<OGLFont Verdana 56  ITALIC[NIL]>
#<OGLFont Verdana 56  BOLD ITALIC[NIL]>
#<OGLFont Verdana 64 [NIL]>
#<OGLFont Verdana 64  BOLD[NIL]>
#<OGLFont Verdana 64  ITALIC[NIL]>
#<OGLFont Verdana 64  BOLD ITALIC[NIL]>
