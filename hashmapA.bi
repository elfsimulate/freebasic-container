' ########################################################################################
' 通用 HashMap 模块（开放寻址法 + 线性探测）
' 优化：SoA 布局 + 状态指纹 + 墓碑感知扩容 + 原地墓碑清理
' 作者：ycp, 406192297@qq.com
' 使用方式：define_hashmap(keytype, valuetype, mapname)

' License: Distributed under the MIT license.
'
' Permission is hereby granted, free of charge, to any person obtaining a copy of this
' software and associated documentation files (the "Software"), to deal in the Software
' without restriction, including without limitation the rights to use, copy, modify, merge,
' publish, distribute, sublicense, and/or sell copies of the Software, and to permit
' persons to whom the Software is furnished to do so, subject to the following conditions:

' The above copyright notice and this permission notice shall be included in all copies or
' substantial portions of the Software.

' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
' INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
' PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
' FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
' OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
' DEALINGS IN THE SOFTWARE.'
' ########################################################################################


' =====================================================
' 通用 HashMap 模块（开放寻址法 + 线性探测）—— 性能优化版
' 优化：SoA 布局 + 状态指纹 + 墓碑感知扩容 + 原地墓碑清理
' 作者：ycp, 406192297@qq.com
' =====================================================

#include once "crt/mem.bi"

'==============高性能字符串哈希函数 rapidhash ==============
Dim Shared rapid_secret(0 To 7) As ULongInt = { &H2D358DCCAA6C78A5ull, &H8BB84B93962EACC9ull,&H4B33A62ED433D4A3ull, &H4D5A2DA51DE1AA47ull, _
&HA0761D6478BD642Full,&HE7037ED1A0B428DBull,&H90ED1765281C388Cull, &HAAAAAAAAAAAAAAAAull }

Sub rapid_mum(ByRef A As ULongInt, ByRef B As ULongInt)
    Dim As ULongInt ha = (A Shr 32), hb = (B Shr 32)
    Dim As ULongInt la = A And &HFFFFFFFFull, lb = B And &HFFFFFFFFull
    Dim As ULongInt rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb
    Dim As ULongInt t = (rl + (rm0 Shl 32)), c = (IIf(t < rl, 1, 0))
    Dim As ULongInt lo = t + (rm1 Shl 32)
    c += IIf(lo < t, 1, 0)
    Dim As ULongInt hi = rh + (rm0 Shr 32) + (rm1 Shr 32) + c
    A = lo:        B = hi
End Sub

Function rapid_mix(ByVal A As ULongInt, ByVal B As ULongInt) As ULongInt
    Dim As ULongInt ha = (A Shr 32), hb = (B Shr 32)
    Dim As ULongInt la = A And &HFFFFFFFFull, lb = B And &HFFFFFFFFull
    Dim As ULongInt rh = ha * hb, rm0 = ha * lb, rm1 = hb * la, rl = la * lb
    Dim As ULongInt t = (rl + (rm0 Shl 32)), c = (IIf(t < rl, 1, 0))
    Dim As ULongInt lo = t + (rm1 Shl 32)
    c += IIf(lo < t, 1, 0)
    Dim As ULongInt hi = rh + (rm0 Shr 32) + (rm1 Shr 32) + c
    A = lo:        B = hi
    Function = A Xor B
End Function

Function rapid_read64(ByVal p As UByte Ptr) As ULongInt
    Dim As ULongInt v
    memcpy(@v, p, 8)
    Function = v
End Function

Function rapid_read32(ByVal p As UByte Ptr) As ULongInt
    Dim As ULong v
    memcpy(@v, p, 4)
    Function = v
End Function

Function rapidhashNano_internal( _
        ByVal key As Any Ptr, _
        ByVal lens As UInteger, _
        ByVal seed As ULongInt, _
        ByVal secret As ULongInt Ptr _
        ) As ULongInt
        Dim As UByte Ptr p = key
        seed Xor= rapid_mix(seed Xor secret[2], secret[1])
        Dim As ULongInt a = 0, b = 0
        Dim As UInteger i = lens
        If (lens <= 16) Then
            If lens >= 4 Then
                seed Xor= lens
                If lens >= 8 Then
                    Dim As UByte Ptr plast = p + lens - 8
                    a = rapid_read64(p) : b = rapid_read64(plast)
                Else
                    Dim As UByte Ptr plast = p + lens - 4
                    a = rapid_read32(p) : b = rapid_read32(plast)
                End If
            ElseIf lens > 0 Then
                a = ((CULngInt(p[0]) Shl 45) Or p[lens - 1])
                b = p[lens Shr 1]
            Else
                a = 0 : b = 0
            End If
        Else
            If i > 48 Then
                Dim As ULongInt see1 = seed, see2 = seed
                Do
                    seed = rapid_mix(rapid_read64(p)     Xor secret[0], rapid_read64(p + 8)  Xor seed)
                    see1 = rapid_mix(rapid_read64(p + 16) Xor secret[1], rapid_read64(p + 24) Xor see1)
                    see2 = rapid_mix(rapid_read64(p + 32) Xor secret[2], rapid_read64(p + 40) Xor see2)
                    p += 48 : i -= 48
                Loop While i > 48
                seed Xor= see1 : seed Xor= see2
            End If
            If i > 16 Then
                seed = rapid_mix(rapid_read64(p) Xor secret[2], rapid_read64(p + 8) Xor seed)
                If i > 32 Then
                    seed = rapid_mix(rapid_read64(p + 16) Xor secret[2], rapid_read64(p + 24) Xor seed)
                End If
            End If
            a = rapid_read64(p + i - 16) Xor i
            b = rapid_read64(p + i - 8)
        End If
        a Xor= secret[1]:    b Xor= seed
        rapid_mum(a, b)
        Function = rapid_mix(a Xor secret[7], b Xor secret[1] Xor i)
End Function

Function rapidhashNano(ByVal key As Any Ptr, ByVal keylens As UInteger, ByVal seed As ULongInt=0) As ULongInt
    Function = rapidhashNano_internal(key, keylens, seed, @rapid_secret(0))
End Function

'==============多类型哈希函数重载====================
Declare function HashFunction overload (byref key as integer) as uinteger
Declare Function HashFunction Overload (ByRef key As UInteger) As UInteger
Declare Function HashFunction Overload (ByRef key As LongInt) As UInteger
declare function HashFunction overload (byref key as ulongint) as uinteger
declare function HashFunction overload (byref key as string) as uinteger
declare function HashFunction overload (byref key as single) as uinteger
Declare Function HashFunction Overload (ByRef key As Double) As UInteger
Declare Function HashFunction Overload (ByRef key As ULong) As UInteger

function HashFunction overload (byref key as integer) as uinteger
    dim h as uinteger = cuint(key)
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h xor (h shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h
end function

Function HashFunction Overload (ByRef key As UInteger) As UInteger
    Dim h As UInteger = key
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h xor (h shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h
end function

Function HashFunction Overload (ByRef key As ULong) As UInteger
    Dim h As UInteger = key
    h = (h xor (h shr 16)) * &h85ebca6b
    h = (h Xor (h Shr 13)) * &hc2b2ae35
    h = h xor (h shr 16)
    return h
End Function

function HashFunction overload (byref key as longint) as uinteger
    dim h as ulongint = culngint(key)
    h = (h xor (h shr 30)) * &hbf58476d1ce4e5b9ull
    h = (h xor (h shr 27)) * &h94d049bb133111ebull
    h = h xor (h shr 31)
    return culng(h )
end function

function HashFunction overload (byref key as ulongint) as uinteger
    dim h as ulongint = key
    h = (h xor (h shr 30)) * &hbf58476d1ce4e5b9ull
    h = (h xor (h shr 27)) * &h94d049bb133111ebull
    h = h xor (h shr 31)
    return culng(h )
end function

Function HashFunction Overload (ByRef key As String) As UInteger
    Dim hash64 As ulongint = rapidhashNano(StrPtr(key), len(key))
    return cuint(hash64 )
end function

Function HashFunction Overload (ByRef key As Double) As uinteger
    dim u as ulongint = *cptr(ulongint ptr, @key)
    u = (u xor (u shr 32)) * &h9e3779b97f4a7c15ull
    u = (u xor (u shr 28)) * &hbf58476d1ce4e5b9ull
    u = u xor (u shr 31)
    return culng(u )
End Function

function HashFunction overload (byref key as single) as uinteger
    dim u as ulong = *cptr(ulong ptr, @key)
    u = (u xor (u shr 16)) * &h85ebca6b
    u = (u xor (u shr 13)) * &hc2b2ae35
    u = u xor (u shr 16)
    return u
end function

' =====================================================
' 通用 HashMap 宏（开放寻址 + 线性探测）
' state 字节：bit0-1 状态（0=EMPTY,1=OCCUPIED,2=DELETED），bit2-7 指纹
' =====================================================

#macro DEFINE_HASHMAP(KEY_TYPE, VALUE_TYPE, MAP_NAME)

    Const MAP_NAME##_EMPTY   = 0
    Const MAP_NAME##_DELETED = 2

    Enum MAP_NAME##IteratorState
        MAP_NAME##_ITERATOR_INVALID = 0
        MAP_NAME##_ITERATOR_VALID   = 1
        MAP_NAME##_ITERATOR_END     = 2
    End Enum

    Type MAP_NAME
    Private:
        m_keys        As KEY_TYPE Ptr
        m_vals        As VALUE_TYPE Ptr
        m_state       As UByte Ptr
        m_bucketCount As UInteger
        m_count       As UInteger   ' OCCUPIED 数量
        m_used        As UInteger   ' OCCUPIED + DELETED
        m_loadFactor  As Single
        m_version     As Integer
        m_iterPos     As Integer
        m_iterState   As MAP_NAME##IteratorState
        m_iterVersion As Integer

        Declare Function DefaultHash(ByRef key As KEY_TYPE) As UInteger
        Declare Sub Rehash(newSize As UInteger)
        Declare Function NextPowerOfTwo(ByVal n As UInteger) As UInteger

    Public:
        Declare Constructor()
        Declare Constructor(ByVal initialSize As Integer, ByVal factor As Single = 0.75)
        Declare Destructor()

        Declare Function Add(ByRef key As KEY_TYPE, ByRef value As VALUE_TYPE) As Boolean
        Declare Function Get(ByRef key As KEY_TYPE) As VALUE_TYPE
        Declare Function Find(ByRef key As KEY_TYPE, ByRef value As VALUE_TYPE) As Boolean
        Declare Function Remove(ByRef key As KEY_TYPE) As Boolean
        Declare Function ContainsKey(ByRef key As KEY_TYPE) As Boolean
        Declare Sub Clear()

        Declare Property Size() As UInteger
        Declare Property Capacity() As UInteger
        Declare Property LoadFactor() As Single
        Declare Property IsEmpty() As Boolean

        Declare Function MoveFirst() As Boolean
        Declare Function MoveNext() As Boolean
        Declare Function CurrentKey(ByRef key As KEY_TYPE) As Boolean
        Declare Function CurrentValue(ByRef value As VALUE_TYPE) As Boolean

        Declare Sub ForEach(ByVal callback As Sub(ByVal key As KEY_TYPE, ByVal value As VALUE_TYPE))
        Declare Sub ForEachPtr(ByVal callback As Sub(ByVal key As KEY_TYPE Ptr, ByVal value As VALUE_TYPE Ptr))

        Declare Sub Optimize()
        Declare Sub PrintStatistics()
    End Type

    '---------- 辅助 ----------
    Function MAP_NAME.NextPowerOfTwo(ByVal n As UInteger) As UInteger
        If n = 0 Then Return 1
        n -= 1
        n Or= n Shr 1
        n Or= n Shr 2
        n Or= n Shr 4
        n Or= n Shr 8
        n Or= n Shr 16
        Return n + 1
    End Function

    Function MAP_NAME.DefaultHash(ByRef key As KEY_TYPE) As UInteger
        Return HashFunction(key)
    End Function

    '---------- 构造 ----------
    Constructor MAP_NAME()
        m_bucketCount = 16
        m_keys  = Callocate(m_bucketCount * SizeOf(KEY_TYPE))
        m_vals  = Callocate(m_bucketCount * SizeOf(VALUE_TYPE))
        m_state = Callocate(m_bucketCount)
        If m_keys = 0 Or m_vals = 0 Or m_state = 0 Then
            Print "Error: Failed to allocate hashmap buckets"
        End If
        m_count = 0 : m_used = 0
        m_loadFactor = 0.75
        m_version = 0
        m_iterPos = -1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = 0
    End Constructor

    Constructor MAP_NAME(ByVal initialSize As Integer, ByVal factor As Single = 0.75)
        If initialSize < 1 Then initialSize = 16
        If factor <= 0 Then factor = 0.75
        m_bucketCount = NextPowerOfTwo(initialSize)
        m_keys  = Callocate(m_bucketCount * SizeOf(KEY_TYPE))
        m_vals  = Callocate(m_bucketCount * SizeOf(VALUE_TYPE))
        m_state = Callocate(m_bucketCount)
        If m_keys = 0 Or m_vals = 0 Or m_state = 0 Then
            Print "Error: Failed to allocate hashmap buckets"
        End If
        m_count = 0 : m_used = 0
        m_loadFactor = factor
        m_version = 0
        m_iterPos = -1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = 0
    End Constructor

    '---------- 析构 ----------
    Destructor MAP_NAME()
        Clear()
        If m_keys  <> 0 Then Deallocate(m_keys)  : m_keys  = 0
        If m_vals  <> 0 Then Deallocate(m_vals)  : m_vals  = 0
        If m_state <> 0 Then Deallocate(m_state) : m_state = 0
    End Destructor

    '---------- Rehash（重建三套数组，丢弃墓碑）----------
   Sub MAP_NAME.Rehash(newSize As UInteger)
        newSize = NextPowerOfTwo(newSize)
        If newSize = m_bucketCount AndAlso m_used = m_count Then Exit Sub

        Dim newKeys  As KEY_TYPE Ptr   = Callocate(newSize * SizeOf(KEY_TYPE))
        Dim newVals  As VALUE_TYPE Ptr = Callocate(newSize * SizeOf(VALUE_TYPE))
        Dim newState As UByte Ptr      = Callocate(newSize)
        If newKeys = 0 Or newVals = 0 Or newState = 0 Then
            Print "Warning: Rehash failed"
            If newKeys  <> 0 Then Deallocate(newKeys)
            If newVals  <> 0 Then Deallocate(newVals)
            If newState <> 0 Then Deallocate(newState)
            Exit Sub
        End If

        Dim newMask As UInteger = newSize - 1
        Dim As KEY_TYPE defaultKey
        Dim As VALUE_TYPE defaultValue

        Dim oldSt As UByte Ptr    = m_state
        Dim oldKs As KEY_TYPE Ptr = m_keys
        Dim oldVs As VALUE_TYPE Ptr = m_vals

        For i As UInteger = 0 To m_bucketCount - 1
            Dim s As UByte = oldSt[i]
            If (s And 1) = 1 Then                     ' OCCUPIED：搬到新表
                Dim h  As UInteger = DefaultHash(oldKs[i])
                Dim fp As UByte    = (h Shr 24) And &H3F
                Dim idx As UInteger = h And newMask
                Do
                    If newState[idx] = 0 Then Exit Do
                    idx = (idx + 1) And newMask
                Loop
                newKeys[idx]  = oldKs[i]
                newVals[idx]  = oldVs[i]
                newState[idx] = (fp Shl 2) Or 1
            End If
            ' ★ 关键：OCCUPIED 和 DELETED 都要释放旧 String（延迟释放的补偿）
            If s <> 0 Then
                oldKs[i] = defaultKey
                oldVs[i] = defaultValue
            End If
        Next

        Deallocate(m_keys)
        Deallocate(m_vals)
        Deallocate(m_state)

        m_keys        = newKeys
        m_vals        = newVals
        m_state       = newState
        m_bucketCount = newSize
        m_used        = m_count

        m_version += 1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
    End Sub

    '---------- Clear ----------
    Sub MAP_NAME.Clear()
        If m_state = 0 Then Exit Sub
        Dim As KEY_TYPE defaultKey
        Dim As VALUE_TYPE defaultValue
        Dim st As UByte Ptr    = m_state
        Dim ks As KEY_TYPE Ptr = m_keys
        Dim vs As VALUE_TYPE Ptr = m_vals
        For i As UInteger = 0 To m_bucketCount - 1
            If st[i] <> 0 Then                      
                ks[i] = defaultKey
                vs[i] = defaultValue
            End If
            st[i] = 0
        Next
        m_count = 0 : m_used = 0
        m_version += 1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
    End Sub

    '---------- Add ----------
    Function MAP_NAME.Add(ByRef key As KEY_TYPE, ByRef value As VALUE_TYPE) As Boolean
        ' 先算哈希（与容量无关），避免扩容后重复计算
        Dim h As UInteger = DefaultHash(key)

        ' 扩容：基于 m_used（OCCUPIED + DELETED），防止墓碑累积
        If (m_used + 1) >= m_bucketCount * m_loadFactor Then
            Dim newSize As UInteger = m_bucketCount * 2
            If newSize > &H40000000 Then newSize = &H40000000
            Rehash(newSize)
        End If

        Dim fp   As UByte    = (h Shr 24) And &H3F
        Dim mask As UInteger = m_bucketCount - 1
        Dim idx  As UInteger = h And mask
        Dim firstTomb As Long = -1

        Do
            Dim s As UByte = m_state[idx]
            If s = 0 Then
                ' EMPTY：优先复用 firstTomb
                Dim target As UInteger
                If firstTomb >= 0 Then
                    target = CUInt(firstTomb)
                Else
                    target = idx
                    m_used += 1
                End If
                m_keys[target]  = key
                m_vals[target]  = value
                m_state[target] = (fp Shl 2) Or 1
                m_count += 1
                m_version += 1
                m_iterState = MAP_NAME##_ITERATOR_INVALID
                Return True
            ElseIf s = 2 Then
                ' DELETED：记下第一个墓碑
                If firstTomb < 0 Then firstTomb = idx
            ElseIf (s Shr 2) = fp Then
                ' OCCUPIED 且指纹匹配：再比完整 key
                If m_keys[idx] = key Then
                    m_vals[idx] = value
                    m_version += 1
                    Return True
                End If
            End If
            idx = (idx + 1) And mask
        Loop
    End Function

    '---------- Get ----------
    Function MAP_NAME.Get(ByRef key As KEY_TYPE) As VALUE_TYPE
        Dim v As VALUE_TYPE
        If Find(key, v) Then Return v
        Dim empty As VALUE_TYPE
        Return empty
    End Function

    '---------- Find ----------
    Function MAP_NAME.Find(ByRef key As KEY_TYPE, ByRef value As VALUE_TYPE) As Boolean
        Dim h    As UInteger = DefaultHash(key)
        Dim fp   As UByte    = (h Shr 24) And &H3F
        Dim mask As UInteger = m_bucketCount - 1
        Dim idx  As UInteger = h And mask

        Do
            Dim s As UByte = m_state[idx]
            If s = 0 Then Return False          ' EMPTY
            If (s And 1) = 1 Then               ' OCCUPIED
                If (s Shr 2) = fp Then
                    If m_keys[idx] = key Then
                        value = m_vals[idx]
                        Return True
                    End If
                End If
            End If
            ' DELETED 继续探测
            idx = (idx + 1) And mask
        Loop
        Return False
    End Function

    '---------- Remove ----------
    Function MAP_NAME.Remove(ByRef key As KEY_TYPE) As Boolean
        Dim h    As UInteger = DefaultHash(key)
        Dim fp   As UByte    = (h Shr 24) And &H3F
        Dim mask As UInteger = m_bucketCount - 1
        Dim idx  As UInteger = h And mask
        Dim st   As UByte Ptr    = m_state       ' ★ 局部化，省掉每次字段读取
        Dim ks   As KEY_TYPE Ptr = m_keys

        Do
            Dim s As UByte = st[idx]
            If s = 0 Then Return False
            If (s And 1) = 1 Then
                If (s Shr 2) = fp Then
                    If ks[idx] = key Then
                        st[idx] = 2                ' ★ 只标记，不析构 String
                        m_count -= 1
                        m_version += 1
                        m_iterState = MAP_NAME##_ITERATOR_INVALID
                        Return True
                    End If
                End If
            End If
            idx = (idx + 1) And mask
        Loop
    End Function

    '---------- ContainsKey ----------
    Function MAP_NAME.ContainsKey(ByRef key As KEY_TYPE) As Boolean
        Dim h    As UInteger = DefaultHash(key)
        Dim fp   As UByte    = (h Shr 24) And &H3F
        Dim mask As UInteger = m_bucketCount - 1
        Dim idx  As UInteger = h And mask

        Do
            Dim s As UByte = m_state[idx]
            If s = 0 Then Return False
            If (s And 1) = 1 Then
                If (s Shr 2) = fp Then
                    If m_keys[idx] = key Then Return True
                End If
            End If
            idx = (idx + 1) And mask
        Loop
        Return False
    End Function

    '---------- 属性 ----------
    Property MAP_NAME.Size() As UInteger
        Return m_count
    End Property

    Property MAP_NAME.Capacity() As UInteger
        Return m_bucketCount
    End Property

    Property MAP_NAME.LoadFactor() As Single
        If m_bucketCount = 0 Then Return 0.0
        Return m_count / m_bucketCount
    End Property

    Property MAP_NAME.IsEmpty() As Boolean
        Return m_count = 0
    End Property

    '---------- 迭代器 ----------
    Function MAP_NAME.MoveFirst() As Boolean
        m_iterPos = -1
        m_iterState = MAP_NAME##_ITERATOR_INVALID
        m_iterVersion = m_version
        Return MoveNext()
    End Function

    Function MAP_NAME.MoveNext() As Boolean
        If m_iterVersion <> m_version Then
            m_iterState = MAP_NAME##_ITERATOR_INVALID
            Return False
        End If
        If m_iterState = MAP_NAME##_ITERATOR_END Then Return False

        m_iterPos += 1
        While m_iterPos < m_bucketCount
            If (m_state[m_iterPos] And 1) = 1 Then
                m_iterState = MAP_NAME##_ITERATOR_VALID
                Return True
            End If
            m_iterPos += 1
        Wend
        m_iterState = MAP_NAME##_ITERATOR_END
        m_iterPos = m_bucketCount
        Return False
    End Function

    Function MAP_NAME.CurrentKey(ByRef key As KEY_TYPE) As Boolean
        If m_iterState = MAP_NAME##_ITERATOR_VALID And _
           m_iterVersion = m_version And _
           m_iterPos >= 0 And m_iterPos < m_bucketCount Then
            If (m_state[m_iterPos] And 1) = 1 Then
                key = m_keys[m_iterPos]
                Return True
            End If
        End If
        Return False
    End Function

    Function MAP_NAME.CurrentValue(ByRef value As VALUE_TYPE) As Boolean
        If m_iterState = MAP_NAME##_ITERATOR_VALID And _
           m_iterVersion = m_version And _
           m_iterPos >= 0 And m_iterPos < m_bucketCount Then
            If (m_state[m_iterPos] And 1) = 1 Then
                value = m_vals[m_iterPos]
                Return True
            End If
        End If
        Return False
    End Function

    '---------- 遍历 ----------
    Sub MAP_NAME.ForEach(ByVal callback As Sub(ByVal key As KEY_TYPE, ByVal value As VALUE_TYPE))
        Dim ver As Integer = m_version
        For i As UInteger = 0 To m_bucketCount - 1
            If ver <> m_version Then Exit For
            If (m_state[i] And 1) = 1 Then
                callback(m_keys[i], m_vals[i])
            End If
        Next
    End Sub

    Sub MAP_NAME.ForEachPtr(ByVal callback As Sub(ByVal key As KEY_TYPE Ptr, ByVal value As VALUE_TYPE Ptr))
        Dim ver As Integer = m_version
        For i As UInteger = 0 To m_bucketCount - 1
            If ver <> m_version Then Exit For
            If (m_state[i] And 1) = 1 Then
                callback(@m_keys[i], @m_vals[i])
            End If
        Next
    End Sub

    '---------- Optimize ----------
    Sub MAP_NAME.Optimize()
        If m_loadFactor <= 0 Then Exit Sub
        Dim target As UInteger
        If m_count = 0 Then
            target = 16
        Else
            Dim ratio As Single = m_count / m_loadFactor
            If ratio > &H3FFFFFFF Then ratio = &H3FFFFFFF
            target = NextPowerOfTwo(CUInt(ratio) + 1)
            If target < 16 Then target = 16
        End If
        Rehash(target)
    End Sub

    '---------- 统计 ----------
    Sub MAP_NAME.PrintStatistics()
        print "primehashmap v2 "
        Print "=== HashMap Statistics ==="
        Print "Size: " & m_count
        Print "Capacity: " & m_bucketCount
        Print "Load Factor: " & LoadFactor
        Print "Threshold: " & m_loadFactor

        Dim maxCluster As Integer = 0
        Dim emptySlots As Integer = 0
        Dim deletedSlots As Integer = 0
        Dim totalOccupied As Integer = 0
        Dim currentCluster As Integer = 0

        For i As UInteger = 0 To m_bucketCount - 1
            If m_state[i] = 0 Then
                emptySlots += 1
                If currentCluster > maxCluster Then maxCluster = currentCluster
                currentCluster = 0
            Else
                currentCluster += 1
                If (m_state[i] And 1) = 1 Then
                    totalOccupied += 1
                Else
                    deletedSlots += 1
                End If
            End If
        Next
        If currentCluster > maxCluster Then maxCluster = currentCluster
 
        Print "Empty slots: " & emptySlots & " (" & (emptySlots * 100 \ m_bucketCount) & "%)"
        Print "Deleted slots: " & deletedSlots & " (" & (deletedSlots * 100 \ m_bucketCount) & "%)"
        Print "Max cluster length: " & maxCluster
        If (m_bucketCount - emptySlots) > 0 Then
            Print "Average occupancy per cluster: " & (totalOccupied / (m_bucketCount - emptySlots))
        End If
        
        Print "=========================="
    End Sub

#endmacro
